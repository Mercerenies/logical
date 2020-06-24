
module Language.Logic.StdLib.Arithmetic(arithFunctions, evalArithWith, evalArith) where

import Language.Logic.Term
import Language.Logic.Number(Number(..))
import Language.Logic.Error

import Polysemy
import Polysemy.Error

import Data.Map(Map)
import qualified Data.Map as Map

type ArithFns = Map String ([Number] -> Either RuntimeError Number)

throwArith :: String -> Either RuntimeError a
throwArith = Left . ArithmeticError

_arg0 :: [Number] -> Either RuntimeError ()
_arg0 [] = pure ()
_arg0 _ = throwArith "Expecting zero arguments"

_arg1 :: (Number -> a) -> [Number] -> Either RuntimeError a
_arg1 f [x] = pure (f x)
_arg1 _ _ = throwArith "Expecting one argument"

arg2 :: (Number -> Number -> a) -> [Number] -> Either RuntimeError a
arg2 f [x, y] = pure $ f x y
arg2 _ _ = throwArith "Expecting two arguments"

arg1or2 :: (Number -> a) -> (Number -> Number -> a) -> [Number] -> Either RuntimeError a
arg1or2 f g xs = case xs of
                   [x] -> pure $ f x
                   [x, y] -> pure $ g x y
                   _ -> throwArith "Expecting one or two arguments"

fromBool :: Num a => Bool -> a
fromBool False = 0
fromBool True  = 1

arithFunctions :: ArithFns
arithFunctions = Map.fromList [
                  ("+", arg1or2 id (+)),
                  ("-", arg1or2 negate (-)),
                  ("*", arg2 (*)),
                  ("/", arg2 (/)),
                  ("<", arg2 $ \x y -> fromBool $ x < y),
                  (">", arg2 $ \x y -> fromBool $ x > y),
                  ("<=", arg2 $ \x y -> fromBool $ x <= y),
                  (">=", arg2 $ \x y -> fromBool $ x >= y),
                  ("==", arg2 $ \x y -> fromBool $ x == y),
                  ("!=", arg2 $ \x y -> fromBool $ x /= y)
                 ]

evalArithWith :: Member (Error RuntimeError) r => ArithFns -> Term -> Sem r Number
evalArithWith _ (TermNum n) = pure n
evalArithWith _ (TermVar s) = throw (VarsNotDone [s])
evalArithWith fns (TermCompound h ts) =
    case Map.lookup h fns of
      Nothing -> throw $ ArithmeticError $ "No such function " ++ show h
      Just f -> mapM (evalArithWith fns) ts >>= fromEither . f

evalArith :: Member (Error RuntimeError) r => Term -> Sem r Number
evalArith = evalArithWith arithFunctions