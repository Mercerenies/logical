
module Language.Logic.Parser.Token(TokenPos(..), Token(..), Spec(..), Keyword(..),
                                   oneToken, readTokens,
                                   satisfy, satisfyTok,
                                   var, integer, ratio, float, atom, litAtom, operator, special, keyword) where

import Text.Parsec hiding (many, (<|>), satisfy, spaces)
import qualified Text.Parsec as P

import Data.Char
import Data.Ratio
import Data.Maybe
import Control.Monad
import Control.Applicative
import qualified Text.Read as Read

type Parser = Parsec String Int

-- TODO Prettier Show instances for Token and TokenPos

data TokenPos = TokenPos SourcePos Int Token
                deriving (Eq, Ord, Show)

data Token = TokenVar String
           | TokenInt Integer
           | TokenRat Rational
           | TokenFloat Double
           | TokenAtom String
           | TokenOperator String
           | TokenSpecial Spec
           | TokenKeyword Keyword
             deriving (Eq, Ord, Show)

data Spec = OpenParen | CloseParen | Colon | Semicolon | Dot | Comma |
            OpenBrace | CloseBrace
            deriving (Eq, Ord, Enum, Show)

data Keyword = Operator
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

keywords :: [String]
keywords = ["operator"]

pkeyword :: Parser Keyword
pkeyword = Operator <$ string "operator"

sign :: Num a => Parser (a -> a)
sign = plus <|> minus <|> pure id
    where plus = id <$ char '+'
          minus = negate <$ char '-'

pinteger :: Parser Integer
pinteger = try $ do
  sgn <- sign
  value <- read <$> many1 digit
  return $ sgn value

pratio :: Parser Rational
pratio = try $ do
  sgn <- sign
  num <- read <$> many1 digit
  _ <- P.satisfy (`elem` "Rr")
  den <- read <$> many1 digit
  when (den == 0) $ fail "denominator is zero"
  return $ sgn (num % den)

pfloat :: Parser Double
pfloat = try $ do
  intpart <- show <$> pinteger
  decpart <- optionMaybe ((:) <$> char '.' <*> many1 digit)
  exppart <- optionMaybe ((:) <$> P.satisfy (`elem` "Ee") <*> (show <$> pinteger))
  when (isNothing decpart && isNothing exppart) $ fail "not a floating-point literal"
  let str = intpart ++ maybe "" id decpart ++ maybe "" id exppart
  case Read.readMaybe str of
    Nothing -> fail "not a floating-point literal"
    Just x -> pure x

identifier :: Parser String
identifier = try $ do
     res <- liftA2 (:) startChar (many idChar)
     guard $ not (res `elem` keywords)
     return res
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

-- TODO I'm debating what to do with $. Haskell treats it as an
-- operator, but many other languages (Scala, Java, Javascript, etc.)
-- treat it as a standard identifier character.

-- TODO Also, we definitely want to support Unicode in operators. I
-- plan to do so later, once we've finalized more of the syntax and
-- decided which Unicode categories make sense to be used in
-- operators.

operChar :: Parser Char
operChar = P.satisfy (`elem` "!%&*+/<=>?@\\^|-~")

poper :: Parser String
poper = many1 operChar

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
                TokenRat <$> pratio <|>
                TokenFloat <$> pfloat <|>
                TokenInt <$> pinteger <|>
                TokenAtom <$> (patom <|> patomQuoted) <|>
                TokenOperator <$> poper <|>
                TokenSpecial <$> pspecial <|>
                TokenKeyword <$> pkeyword

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

ratio :: Stream s m TokenPos => ParsecT s Int m Rational
ratio = satisfyTok $ \case
        TokenRat r -> Just r
        _ -> Nothing

float :: Stream s m TokenPos => ParsecT s Int m Double
float = satisfyTok $ \case
        TokenFloat d -> Just d
        _ -> Nothing

atom :: Stream s m TokenPos => ParsecT s Int m String
atom = satisfyTok $ \case
       TokenAtom s -> Just s
       _ -> Nothing

litAtom :: Stream s m TokenPos => String -> ParsecT s Int m String
litAtom s = try (atom >>= \s' -> guard (s == s') >> return s')

operator :: Stream s m TokenPos => ParsecT s Int m String
operator = satisfyTok $ \case
           TokenOperator s -> Just s
           _ -> Nothing

special :: Stream s m TokenPos => Spec -> ParsecT s Int m Spec
special s = satisfyTok $ \case
            TokenSpecial s' | s == s' -> Just s'
            _ -> Nothing

keyword :: Stream s m TokenPos => Keyword -> ParsecT s Int m Keyword
keyword s = satisfyTok $ \case
            TokenKeyword s' | s == s' -> Just s'
            _ -> Nothing
