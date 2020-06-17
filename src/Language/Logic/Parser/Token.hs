
module Language.Logic.Parser.Token(TokenPos(..), Token(..), Spec(..),
                                   oneToken, readTokens,
                                   satisfy, satisfyTok,
                                   var, integer, atom, special) where

import Text.Parsec hiding (many, (<|>), satisfy, spaces)
import qualified Text.Parsec as P

import Data.Char
import Control.Monad
import Control.Applicative

type Parser = Parsec String Int

-- TODO Prettier Show instances for Token and TokenPos

data TokenPos = TokenPos SourcePos Int Token
                deriving (Eq, Ord, Show)

data Token = TokenVar String
           | TokenInt Integer
           | TokenAtom String
           | TokenSpecial Spec
             deriving (Eq, Ord, Show)

data Spec = OpenParen | CloseParen | Colon | Semicolon | Dot | Comma |
            OpenBrace | CloseBrace
            deriving (Eq, Ord, Enum, Show)

pspecial :: Parser Spec
pspecial = OpenParen  <$ char '(' <|>
           CloseParen <$ char ')' <|>
           Colon      <$ char ':' <|>
           Semicolon  <$ char ';' <|>
           Dot        <$ char '.' <|>
           Comma      <$ char ',' <|>
           OpenBrace  <$ char '{' <|>
           CloseBrace <$ char '}'

sign :: Num a => Parser (a -> a)
sign = plus <|> minus <|> pure id
    where plus = id <$ char '+'
          minus = negate <$ char '-'

pinteger :: Parser Integer
pinteger = do
  sgn <- sign
  value <- read <$> many1 digit
  return $ sgn value

identifier :: Parser String
identifier = liftA2 (:) startChar (many idChar)
    where startChar = letter <|> char '_'
          idChar = startChar <|> digit

patom :: Parser String
patom = try $ do
  xs <- identifier
  guard (case xs of
           (x:_) | isLower x -> True
           _ -> False)
  return xs

patomQuoted :: Parser String
patomQuoted = do
  _ <- char '`'
  xs <- many (P.satisfy (/= '`'))
  _ <- char '`'
  return xs

pvar :: Parser String
pvar = try $ do
  xs <- identifier
  guard (case xs of
           (x:_) | isLower x -> False
           _ -> True)
  return xs

spaceNotNewline :: Monad m => ParsecT String Int m Char
spaceNotNewline = P.satisfy (\x -> isSpace x && not (x == '\n'))

lineComment :: Monad m => ParsecT String Int m ()
lineComment = void $ char '#' >> many (P.satisfy (/= '\n')) >> lookAhead (char '\n')

spaces :: Monad m => ParsecT String Int m ()
spaces = skipMany (void spaceNotNewline <|> newlineChecked <|> lineComment)
    where newlineChecked = do
            _ <- char '\n'
            indent <- length <$> many spaceNotNewline
            putState indent
            pure ()

--nextline :: Parser Int
--nextline = endOfLine *> (length <$> many spaceNotNewline)

oneToken :: Parser TokenPos
oneToken = TokenPos <$> getPosition <*> getState <*> tok
    where tok = TokenVar <$> pvar <|>
                TokenInt <$> pinteger <|>
                TokenAtom <$> (patom <|> patomQuoted) <|>
                TokenSpecial <$> pspecial

readTokens :: String -> String -> Either ParseError [TokenPos]
readTokens = runP (many (spaces *> oneToken <* spaces) <* eof) 0

-- For satisfy and all functions derived from it, the user state in
-- Parsec is the current indentation level.

satisfy :: Stream s m TokenPos => (TokenPos -> Maybe a) -> ParsecT s Int m a
satisfy p = tokenPrim show nextPos (\x -> (,) x <$> p x) >>= \(tok, a) -> handleIndent tok >> pure a
    where nextPos _ (TokenPos pos _ _) _ = pos
          handleIndent (TokenPos _ n _) = putState n

satisfyTok :: Stream s m TokenPos => (Token -> Maybe a) -> ParsecT s Int m a
satisfyTok p = satisfy (p . extractTok)
    where extractTok (TokenPos _ _ x) = x

var :: Stream s m TokenPos => ParsecT s Int m String
var = satisfyTok $ \case
      TokenVar s -> Just s
      _ -> Nothing

integer :: Stream s m TokenPos => ParsecT s Int m Integer
integer = satisfyTok $ \case
          TokenInt n -> Just n
          _ -> Nothing

atom :: Stream s m TokenPos => ParsecT s Int m String
atom = satisfyTok $ \case
       TokenAtom s -> Just s
       _ -> Nothing

special :: Stream s m TokenPos => Spec -> ParsecT s Int m Spec
special s = satisfyTok $ \case
            TokenSpecial s' | s == s' -> Just s'
            _ -> Nothing
