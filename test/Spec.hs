
module Main where

import qualified Language.Logic.Test.Unify
import qualified Language.Logic.Test.Term
import qualified Language.Logic.Test.Files
import qualified Language.Logic.Test.SymbolTable
import qualified Language.Logic.Test.SymbolTable.Monad
import qualified Language.Logic.Test.Tagged

import Test.HUnit

coreTests :: [Test]
coreTests = [Language.Logic.Test.Unify.tests, Language.Logic.Test.Term.tests, Language.Logic.Test.SymbolTable.tests,
             Language.Logic.Test.SymbolTable.Monad.tests, Language.Logic.Test.Tagged.tests]

vmTests :: IO Test
vmTests = Language.Logic.Test.Files.allTestFiles

main :: IO ()
main = do
  vm <- vmTests
  let tests = TestList (coreTests ++ [vm])
  result <- runTestTT tests
  print result
