
module Main where

import Language.Logic.Eval
import Language.Logic.Term
import Language.Logic.Code
import Language.Logic.Parser
import Language.Logic.StdLib

import qualified Data.Map as Map
import System.Environment
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Please provide an input filename" >> exitFailure
    (fname:_) -> do
        contents <- readFile fname
        case tokenizeAndParse fname contents of
          Left err -> print err >> exitFailure
          Right clauses -> do
              prelude <- getPrelude
              let body = prelude <> consolidateClauses clauses
              runProgram body >>= \case
                Left err -> print err >> exitFailure
                Right () -> pure ()

--print $ runProgram example

{-
foo(1).
foo(2).
foo(3).

bar(X) :- foo(X).

main() :- bar(X).
-}

example :: CodeBody
example = CodeBody (Map.fromList clauses)
    where clauses = [
            ("foo", [StdClause (Fact "foo" [TermNum 1]) [],
                     StdClause (Fact "foo" [TermNum 2]) [],
                     StdClause (Fact "foo" [TermNum 3]) []]),
            ("bar", [StdClause (Fact "bar" [TermVar "X"]) [Fact "foo" [TermVar "X"]]]),
            ("main", [StdClause (Fact "main" []) [Fact "bar" [TermVar "X"]]])
           ]
