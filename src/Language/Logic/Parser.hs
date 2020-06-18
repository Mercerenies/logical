
module Language.Logic.Parser(parseText, tokenizeAndParse) where

import Language.Logic.Term
import Language.Logic.Code
import Language.Logic.Parser.Token
import qualified Language.Logic.Parser.Op as Op

import Text.Parsec hiding (satisfy)
import Control.Monad
import Control.Applicative(liftA2)
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.Map as Map

-- Note: The parser state is an Int representing the current
-- indentation level. The syntax used to be indentation-sensitive, and
-- I figure we might as well leave the functionality in in case we
-- need it later. It's not currently used by the parser.

type Parser = Parsec [TokenPos] Int

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
       TermInt <$> integer <|>
       (special OpenParen *> term' <* special CloseParen) <|>
       uncurry TermCompound <$> compoundTerm'

term' :: Parser Term
term' = do
  firstterm <- Op.Term <$> term
  restterms <- concat <$> many (liftA2 (\xs y -> fmap Op.OpTerm xs ++ [Op.Term y]) (many1 operator) term)
  let optable = Op.OpTable Map.empty
      comp = Op.TermComp (\a s b -> TermCompound s [a, b]) (\s a -> TermCompound s [a])
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

clause :: Parser Clause
clause = simpleClause <|> condClause

simpleClause :: Parser Clause
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
condClause :: Parser Clause
condClause = do
  fct <- try (fact <* special Colon)
  inner <- fact
  _ <- special Dot
  return $ StdClause fct [inner]

-- clauseBody :: Parser [Fact]
-- clauseBody = sepBy fact (special Semicolon)

topLevelClauses :: Parser [Clause]
topLevelClauses = many clause

parseText :: String -> [TokenPos] -> Either ParseError [Clause]
parseText = runP (topLevelClauses <* eof) 0

tokenizeAndParse :: String -> String -> Either ParseError [Clause]
tokenizeAndParse src = readTokens src >=> parseText src
