{-# LANGUAGE RecordWildCards #-}

module Main where

import Language.Logic.Eval
import Language.Logic.Term
import Language.Logic.Code
import Language.Logic.Parser
import Language.Logic.StdLib
import Language.Logic.Compile
import Language.Logic.CmdArgs
import Language.Logic.SymbolTable
import Language.Logic.SymbolTable.Monad
import Language.Logic.Var(replaceUnderscores')

import Polysemy

import qualified Data.Map as Map
import System.Exit
import Control.Monad

main :: IO ()
main = do
  CmdArgs {..} <- parseCmd
  let sym = emptyTable
  contents <- readFile cmdFileName
  (prelude, op, sym') <- getPrelude sym
  let (vm, sym'') = getVMData sym'
  case tokenizeAndParse op sym'' cmdFileName contents of
    Left err -> print err >> exitFailure
    Right (clauses, _, sym''') -> do
        let clauses' = consolidateClauses clauses
            (sym'''', clauses'') = run $ runSymbolTableState sym''' (compileBody clauses')
            clauses''' = mapClauses replaceUnderscores' clauses''
            body = prelude <> clauses'''
        when cmdDebugKB $ print clauses'''
        runProgram vm sym'''' cmdDebugLevel body >>= \case
          Left err -> print err >> exitFailure
          Right _ -> pure ()

--print $ runProgram example

{-
foo(1).
foo(2).
foo(3).

bar(X) :- foo(X).

main() :- bar(X).
-}

example :: CodeBody String Fact
example = CodeBody (Map.fromList clauses)
    where clauses = [
            ("foo", [StdClause (Fact "foo" [TermNum 1]) [],
                     StdClause (Fact "foo" [TermNum 2]) [],
                     StdClause (Fact "foo" [TermNum 3]) []]),
            ("bar", [StdClause (Fact "bar" [TermVar "X"]) [Fact "foo" [TermVar "X"]]]),
            ("main", [StdClause (Fact "main" []) [Fact "bar" [TermVar "X"]]])
           ]
