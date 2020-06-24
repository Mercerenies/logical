{-# LANGUAGE OverloadedStrings #-}

module Language.Logic.Test.SymbolTable where

import Language.Logic.SymbolTable

import Test.HUnit
import qualified Data.Text as T
import Data.List(foldl')

tests :: Test
tests = TestLabel "Language.Logic.Test.SymbolTable" $
        TestList [testNotEqual]

internSeveral :: [T.Text] -> SymbolTable -> SymbolTable
internSeveral xs s0 = foldl' (\s t -> snd $ internInTable s t) s0 xs

internForId :: SymbolTable -> T.Text -> SymbolId
internForId s t = fst $ internInTable s t

testNotEqual :: Test
testNotEqual = TestLabel "testNotEqual" $ TestList [
    TestCase (assertBool message $ internForId table "foo" /= internForId table "bar"),
    TestCase (assertBool message $ internForId table "baz" /= internForId table "=:"),
    TestCase (assertBool message $ internForId table "baz" /= internForId table "100"),
    TestCase (assertBool message $ internForId table "baz" /= internForId table "not_in_the_table"),
    TestCase (assertBool message $ internForId table "not_in_the_table" /= internForId table "bar")
  ]
    where table = internSeveral ["foo", "bar", "baz", "=:", "100"] emptyTable
          message = "Two distinct symbols had same ID"
