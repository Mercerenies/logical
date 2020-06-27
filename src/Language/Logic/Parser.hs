
module Language.Logic.Parser(parseText, tokenizeAndParse) where

import Language.Logic.Term
import Language.Logic.Code
import Language.Logic.Decl
import Language.Logic.Parser.Token
import qualified Language.Logic.Parser.Op as Op
import Language.Logic.Number(Number(..))
import Language.Logic.SymbolTable

import Text.Parsec hiding (satisfy, runParser)
import Control.Monad
import Control.Monad.Trans.RWS hiding (ask)
import Control.Monad.Reader
import Control.Applicative(liftA2)
import Data.List(foldl')
import Data.Maybe(mapMaybe)
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.Map as Map

-- Note: The parser state is an Int representing the current
-- indentation level. The syntax used to be indentation-sensitive, and
-- I figure we might as well leave the functionality in in case we
-- need it later. It's not currently used by the parser.

-- It would be kind of nice if this was a Polysemy effect since we're
-- using Polysemy everywhere else. But Parsec is pretty obviously
-- built with mtl in mind. We could hypothetically embed all of our
-- effects using Polysemy, but that would be messy and introduce a ton
-- of boilerplate, so it's just simpler to use a small mtl-style monad
-- stack here.
type Parser = ParsecT [TokenPos] Int (RWS Op.OpTable () SymbolTable)

{-
currentIndent :: Parser Int
currentIndent = getState

nextIndent :: Parser Int
nextIndent =
    -- Try looking ahead one token. If there isn't a next token, fall
    -- back and get current indent level.
    try (lookAhead (satisfy (const (Just ())) >> getState)) <|> getState
-}

compoundTerm :: Parser (String, [Term])
compoundTerm = do
  head_ <- atom
  body <- option [] $ do
    _ <- special OpenParen
    body <- sepBy term' (special Comma)
    _ <- special CloseParen
    return body
  return (head_, body)

compoundTerm' :: Parser (String, [Term])
compoundTerm' = block <|> compoundTerm

term :: Parser Term
term = TermVar <$> var <|>
       (TermNum . fromInteger) <$> integer <|>
       (TermNum . NumRat) <$> ratio <|>
       (TermNum . NumFloat) <$> float <|>
       (special OpenParen *> term' <* special CloseParen) <|>
       uncurry TermCompound <$> compoundTerm'

term' :: Parser Term
term' = do
  firstterm <- Op.Term <$> term
  restterms <- concat <$> many (liftA2 (\xs y -> fmap Op.OpTerm xs ++ [Op.Term y]) (many1 operator) term)
  optable <- ask
  let comp = Op.TermComp (\a s b -> TermCompound s [a, b]) (\s a -> TermCompound s [a])
      result = Op.resolvePrec optable comp (firstterm :| restterms)
  either (fail . show) pure result

fact :: Parser Fact
fact = term' >>= \case
       TermCompound h ts -> pure $ Fact h ts
       x -> unexpected (show x)

block :: Parser (String, [Term])
block = do
  _ <- special OpenBrace
  let f2t (Fact a b) = TermCompound a b
  terms <- many (f2t <$> fact <* special Semicolon)
  _ <- special CloseBrace
  return ("block", terms)

clause :: Parser (Clause Fact)
clause = simpleClause <|> condClause

simpleClause :: Parser (Clause Fact)
simpleClause = fmap (\fct -> StdClause fct []) $ try (fact <* special Dot)

{-
determineTargetIndent :: Int -> Int -> Maybe Int
determineTargetIndent curr next =
    case curr `compare` next of
      LT -> Just next -- Standard block (indented)
      EQ -> Nothing -- Inline block (TODO Not currently supported)
      GT -> Nothing -- Immediate dedent following (definitely an error)

-- The EQ case above is not currently supported. I'd like to be able
-- to support stuff like this on one line.
--
-- foo(X): bar(X)
--
-- Where the inner block simply ends at the next newline. But there's
-- no way to do that with the current tokenizer.
-}

-- TODO Consider desugaring blocks when parsed as inner here to simply
-- be a list of facts.
condClause :: Parser (Clause Fact)
condClause = do
  fct <- try (fact <* special Colon)
  inner <- fact
  _ <- special Dot
  return $ StdClause fct [inner]

decl :: Parser Decl
decl = operatorDecl

-- TODO Omit associativity for prefix operators (it's ignored anyway)
operatorDecl :: Parser Decl
operatorDecl = do
  _ <- keyword Operator
  fx <- fixity
  name <- operator
  _ <- special Colon
  as <- assoc
  pr <- fromInteger <$> integer
  _ <- special Dot
  return $ OperatorDecl (Op.OpA fx name) (Op.Op pr as)

fixity :: Parser Op.Fixity
fixity = (Op.Infix <$ litAtom "infix") <|>
         (Op.Prefix <$ litAtom "prefix")

assoc :: Parser Op.Assoc
assoc = (Op.AssocLeft <$ litAtom "left") <|>
        (Op.AssocRight <$ litAtom "right")

-- clauseBody :: Parser [Fact]
-- clauseBody = sepBy fact (special Semicolon)

clauseOrDecl :: Parser ClauseOrDecl
clauseOrDecl = (Clause <$> clause) <|>
               (Decl <$> decl)

topLevelClauses :: Parser [ClauseOrDecl]
topLevelClauses = many clauseOrDecl

-- TODO What if we try to change the precedence of an existing
-- operator? Is that an error? Perhaps a warning?
handleOpDecls :: Decl -> Op.OpTable -> Op.OpTable
handleOpDecls (OperatorDecl opa op) (Op.OpTable m) = Op.OpTable (Map.insert opa op m)

runParser :: Parser a -> String -> [TokenPos] -> Op.OpTable -> SymbolTable -> Either ParseError (a, SymbolTable)
runParser p src toks op sym =
    let p' = runPT p 0 src toks
        (res, sym', ()) = runRWS p' op sym in
    fmap (\r -> (r, sym')) res

-- It's not ideal, but currently we parse the text twice. The first
-- time, we parse with no operator table (thus, operator expressions
-- will be parenthesized incorrectly but will still parse), and we
-- extract all of the operator precedence declarations to build up an
-- operator table. The second time, we parse with the correct operator
-- table and discard operator declarations, leaving only clauses
-- containing correctly parenthesized operator expressions.
parseText :: Op.OpTable -> SymbolTable -> String -> [TokenPos] ->
             Either ParseError ([Clause Fact], Op.OpTable, SymbolTable)
parseText table0 sym srcname toks = do
  (firstParse, sym') <- runParser (topLevelClauses <* eof) srcname toks table0 sym
  let table1 = foldl' (flip handleOpDecls) table0 $ mapMaybe toDecl firstParse
  (secondParse, sym'') <- runParser (topLevelClauses <* eof) srcname toks table1 sym'
  return (mapMaybe toClause secondParse, table1, sym'')

tokenizeAndParse :: Op.OpTable -> SymbolTable -> String -> String ->
                    Either ParseError ([Clause Fact], Op.OpTable, SymbolTable)
tokenizeAndParse op sym src = readTokens src >=> parseText op sym src
