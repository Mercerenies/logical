
module Main where

import qualified Language.Logic.Test.Unify
import qualified Language.Logic.Test.Term
import qualified Language.Logic.Test.Files

import Test.HUnit

coreTests :: [Test]
coreTests = [Language.Logic.Test.Unify.tests, Language.Logic.Test.Term.tests]

vmTests :: IO Test
vmTests = Language.Logic.Test.Files.allTestFiles

main :: IO ()
main = do
  vm <- vmTests
  let tests = TestList (coreTests ++ [vm])
  result <- runTestTT tests
  print result
