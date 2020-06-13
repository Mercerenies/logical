
module Main where

import qualified Language.Logic.Test.Unify

import Test.HUnit

coreTests :: [Test]
coreTests = [Language.Logic.Test.Unify.tests]

main :: IO ()
main = do
  let tests = TestList $ coreTests
  result <- runTestTT tests
  print result
