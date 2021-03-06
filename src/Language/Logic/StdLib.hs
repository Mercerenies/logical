
module Language.Logic.StdLib(EvalCtx', stdlib, getPrelude, getVMData) where

import Language.Logic.Code
import Language.Logic.Compile
import Language.Logic.Term
import Language.Logic.Term.Compiled
import Language.Logic.Term.Handle
import Language.Logic.Parser
import Language.Logic.Parser.Op(OpTable(..))
import Language.Logic.Choice
import Language.Logic.Number(Number())
import Language.Logic.Error
import Language.Logic.Eval
import Language.Logic.Tagged
import Language.Logic.StdLib.Arithmetic
import Language.Logic.StdLib.Util
import Language.Logic.VMData
import Language.Logic.SymbolTable(SymbolTable())
import Language.Logic.SymbolTable.Monad
import Language.Logic.GlobalVars(defineGlobal, getGlobal, setGlobal)
import Language.Logic.StdLib.TypeOf(typeOf')
import Language.Logic.Var(replaceUnderscores')
import qualified Language.Logic.Eval.Monad as EM
import qualified Language.Logic.Util as Util
import qualified Language.Logic.StdLib.String as Stdlib.String

import Polysemy
import Polysemy.Error
import qualified Data.Text as T

import Control.Monad
import Control.Applicative
--import Data.Map(Map)
import qualified Data.Map as Map
import Data.Tuple(swap)

writeTerm :: EvalCtx' r => CFact -> Sem r ()
writeTerm = arg1 >=> \t -> EM.writeOut (shows t "\n")

writeString :: EvalCtx' r => CFact -> Sem r ()
writeString = arg1 >=> assertString >=> \t -> EM.writeOut (T.unpack t)

fail_ :: EvalCtx' r => CFact -> Sem r ()
fail_ _ = mzero

call :: EvalCtx' r => CFact -> Sem r ()
call = argsForCall >=> evalGoal

block :: EvalCtx' r => CFact -> Sem r ()
block (CFact _ xs) = mapM assertCompound xs >>= mapM_ evalGoal

typeOfValue :: EvalCtx' r => CFact -> Sem r ()
typeOfValue = arg2 >=> \(term, tyvar) ->
              typeOf' term >>= \ty -> unify tyvar ty

refget :: EvalCtx' r => CFact -> Sem r ()
refget = arg2 >=> \(h, v) ->
         case h of
           CTermHandle (HandleRef h') -> do
                  t <- getGlobal h'
                  unify v t
           _ -> throw (TypeError "reference handle" $ ctermToTerm h)

refset :: EvalCtx' r => CFact -> Sem r ()
refset = arg2 >=> \(h, v) ->
         case h of
           CTermHandle (HandleRef h') -> setGlobal h' $! v
           CTermVar _ -> defineGlobal v >>= \v' -> unify h (CTermHandle (HandleRef v'))
           _ -> throw (TypeError "reference handle or variable" $ ctermToTerm h)

-- add takes three arguments. At least two must be ground. If all
-- three are ground, it verifies that the first two sum to the third.
-- If exactly one is a variable, it completes the arithmetic
-- expression. If any terms are not variables or numbers, a type error
-- is raised.
add :: EvalCtx' r => CFact -> Sem r ()
add = arg3 >=> \case
      (CTermNum   a, CTermNum   b, CTermNum   c) -> guard (a + b == c)
      (CTermIsVar a, CTermNum   b, CTermNum   c) ->
          unify (CTermIsVar a) (CTermNum (c - b))
      (CTermNum   a, CTermIsVar b, CTermNum   c) ->
          unify (CTermIsVar b) (CTermNum (c - a))
      (CTermNum   a, CTermNum   b, CTermIsVar c) ->
          unify (CTermIsVar c) (CTermNum (a + b))
      (a, b, c) -> expectingError valid "variable or number" [a, b, c]
    where valid (CTermVar _) = True
          valid (CTermNum _) = True
          valid _ = False

-- mul operates like add in terms of requiring all arguments to be
-- either numbers or variables and requiring at least two ground
-- arguments. Division by zero will simply produce the appropriate
-- floating point infinity or NaN.
mul :: EvalCtx' r => CFact -> Sem r ()
mul = arg3 >=> \case
      (CTermNum   a, CTermNum   b, CTermNum   c) -> guard (a * b == c)
      (CTermIsVar a, CTermNum   b, CTermNum   c) ->
          unify (CTermIsVar a) (CTermNum (c / b))
      (CTermNum   a, CTermIsVar b, CTermNum   c) ->
          unify (CTermIsVar b) (CTermNum (c / a))
      (CTermNum   a, CTermNum   b, CTermIsVar c) ->
          unify (CTermIsVar c) (CTermNum (a * b))
      (a, b, c) -> expectingError valid "variable or number" [a, b, c]
    where valid (CTermVar _) = True
          valid (CTermNum _) = True
          valid _ = False

evalArith' :: (Member (Error RuntimeError) r, Member VMEnv r) => CTerm -> Sem r Number
evalArith' t = getArithmetic >>= \arith -> evalArithCWith arith t

arithEval :: EvalCtx' r => CFact -> Sem r ()
arithEval = arg2 >=> \(x, t) -> evalArith' t >>= unify x . CTermNum

-- Evaluates the arithmetic expression. If the result is zero, this
-- predicate fails. If not, the predicate succeeds once without
-- unifying anything. The argument must be fully ground.
arithGuard :: EvalCtx' r => CFact -> Sem r ()
arithGuard = arg1 >=> \t -> evalArith' t >>= \t' -> if t' == 0 then mzero else pure ()

-- Evaluates the conditional only once. If the condition succeeds,
-- evaluates the true argument. If it fails, evaluates the false
-- argument. The conditional is only evaluated once, but the other two
-- args are evaluated for all solutions.
if_ :: EvalCtx' r => CFact -> Sem r ()
if_ = arg3 >=> \(c, t, f) -> do
        c' <- assertCompound c
        t' <- assertCompound t
        f' <- assertCompound f
        res <- once $ (True <$ evalGoal c') <|> pure False
        evalGoal (if res then t' else f')

stdlib :: CodeBody String CFact
stdlib = CodeBody $ Map.fromList [
          ("write_term", [
            PrimClause "write_term" (builtinToPrim writeTerm)
           ]),
          ("write_string", [
            PrimClause "write_string" (builtinToPrim writeString)
           ]),
          ("fail", [
            PrimClause "fail" (builtinToPrim fail_)
           ]),
          ("call", [
            PrimClause "call" (builtinToPrim call)
           ]),
          ("=:", [
            PrimClause "=:" (builtinToPrim arithEval)
           ]),
          ("guard", [
            PrimClause "guard" (builtinToPrim arithGuard)
           ]),
          ("if", [
            PrimClause "if" (builtinToPrim if_)
           ]),
          ("block", [
            PrimClause "block" (builtinToPrim block)
           ]),
          ("add", [
            PrimClause "add" (builtinToPrim add)
           ]),
          ("mul", [
            PrimClause "mul" (builtinToPrim mul)
           ]),
          ("type_of", [
            PrimClause "type_of" (builtinToPrim typeOfValue)
           ]),
          ("refget", [
            PrimClause "refget" (builtinToPrim refget)
           ]),
          ("refset", [
            PrimClause "refset" (builtinToPrim refset)
           ]),
          ("string_concat", [
            PrimClause "string_concat" (builtinToPrim Stdlib.String.stringConcat)
           ]),
          ("string_length", [
            PrimClause "string_length" (builtinToPrim Stdlib.String.stringLength)
           ]),
          ("atom_string", [
            PrimClause "atom_string" (builtinToPrim Stdlib.String.atomString)
           ])
         ]

compileLib :: Member (SymbolTableState SymbolId) r =>
              CodeBody String CFact -> Sem r (CodeBody (Tagged Atom SymbolId) CFact)
compileLib (CodeBody m) =
    Util.traverseKeys internTag m >>= traverse (traverse compileClause') >>= return . CodeBody

getPrelude :: SymbolTable -> IO (CodeBody (Tagged Atom SymbolId) CFact, OpTable, SymbolTable)
getPrelude sym = do
  code <- readFile "std/Prelude"
  case tokenizeAndParse (OpTable mempty) sym "std/Prelude" code of
    Left err -> fail (show err)
    Right (clauses, op, sym') ->
        let (sym'', stdlib') = run $ runSymbolTableState sym' (compileLib stdlib)
            (sym''', clauses') = run $ runSymbolTableState sym'' (mapM compileClause clauses)
            clauses'' = fmap replaceUnderscores' clauses' in
        return (stdlib' <> consolidateClauses clauses'', op, sym''')

getVMData :: SymbolTable -> (VMData, SymbolTable)
getVMData sym = swap . run . runSymbolTableState sym $ compile
    where compile = do
            arith <- compileArith arithFunctions
            return (VMData arith)
