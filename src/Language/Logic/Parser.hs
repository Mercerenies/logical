
module Language.Logic.Parser(parseText, tokenizeAndParse) where

import Language.Logic.Term
import Language.Logic.Code
import Language.Logic.Parser.Token

import Text.Parsec hiding (satisfy)
import Control.Monad

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
    body <- sepBy term (special Comma)
    _ <- special CloseParen
    return body
  return (head_, body)

term :: Parser Term
term = TermVar <$> var <|>
       TermInt <$> integer <|>
       uncurry TermCompound <$> compoundTerm

fact :: Parser Fact
fact = uncurry Fact <$> compoundTerm

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
-}

-- The EQ case above is not currently supported. I'd like to be able
-- to support stuff like this on one line.
--
-- foo(X): bar(X)
--
-- Where the inner block simply ends at the next newline. But there's
-- no way to do that with the current tokenizer.

condClause :: Parser Clause
condClause = do
  fct <- try (fact <* special Colon)
  inner <- clauseBody
  _ <- special Dot
  return $ StdClause fct inner

clauseBody :: Parser [Fact]
clauseBody = sepBy fact (special Semicolon)

topLevelClauses :: Parser [Clause]
topLevelClauses = many clause

parseText :: String -> [TokenPos] -> Either ParseError [Clause]
parseText = runP (topLevelClauses <* eof) 0

tokenizeAndParse :: String -> String -> Either ParseError [Clause]
tokenizeAndParse src = readTokens src >=> parseText src
