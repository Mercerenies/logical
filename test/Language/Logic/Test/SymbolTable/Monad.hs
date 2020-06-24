{-# LANGUAGE OverloadedStrings #-}

module Language.Logic.Test.SymbolTable.Monad where

import Language.Logic.SymbolTable.Monad
import Language.Logic.SymbolTable(emptyTable)

import Test.HUnit
import Polysemy
import qualified Data.Text as T

tests :: Test
tests = TestLabel "Language.Logic.Test.SymbolTable.Monad" $
        TestList [testEqual, testNotEqual]

runSymbolTableTest :: Sem '[SymbolTableState Integer, Embed IO] a -> IO a
runSymbolTableTest = runM . evalSymbolTableState emptyTable

assertBool' :: Member (Embed IO) r => String -> Bool -> Sem r ()
assertBool' s b = embed (assertBool s b)

internAll :: Member (SymbolTableState i) r => [T.Text] -> Sem r ()
internAll = mapM_ intern

testEqual :: Test
testEqual = TestLabel "testEqual" $ TestList $ fmap (TestCase . runSymbolTableTest) $ [test1, test2]
    where test1 = do
            x <- intern "foo"
            _ <- intern "bar"
            y <- intern "foo"
            assertBool' "Inconsistent symbol IDs" $ x == y
          test2 = do
            _ <- intern "foo" -- Intern it in advance, then query twice
            _ <- intern "bar"
            x <- intern "foo"
            _ <- intern "baz"
            y <- intern "foo"
            assertBool' "Inconsistent symbol IDs" $ x == y

testNotEqual :: Test
testNotEqual = TestLabel "testNotEqual" $ TestList $ fmap (TestCase . runSymbolTableTest) $ [test1, test2, test3]
    where test1 = do
            internAll ["foo", "bar", "baz", "=:", "100"]
            x <- intern "baz"
            y <- intern "foo"
            assertBool' "Two distinct symbols had same ID" $ x /= y
          test2 = do
            internAll ["foo", "bar", "baz", "=:", "100"]
            x <- intern "=:"
            y <- intern "not_in_the_table"
            assertBool' "Two distinct symbols had same ID" $ x /= y
          test3 = do
            x <- intern "abc"
            y <- intern "def"
            assertBool' "Two distinct symbols had same ID" $ x /= y
