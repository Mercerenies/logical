
module Main where

import qualified Language.Logic.Test.Unify
import qualified Language.Logic.Test.Term

import Test.HUnit

coreTests :: [Test]
coreTests = [Language.Logic.Test.Unify.tests, Language.Logic.Test.Term.tests]

main :: IO ()
main = do
  let tests = TestList $ coreTests
  result <- runTestTT tests
  print result
