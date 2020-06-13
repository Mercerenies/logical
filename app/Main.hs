
module Main where

import Language.Logic.Eval
import Language.Logic.Term
import Language.Logic.Code
import Language.Logic.Parser

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
              n <- runProgram (consolidateClauses clauses)
              print n

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
            ("foo", [Clause (Fact "foo" [TermInt 1]) [],
                     Clause (Fact "foo" [TermInt 2]) [],
                     Clause (Fact "foo" [TermInt 3]) []]),
            ("bar", [Clause (Fact "bar" [TermVar "X"]) [Fact "foo" [TermVar "X"]]]),
            ("main", [Clause (Fact "main" []) [Fact "bar" [TermVar "X"]]])
           ]
