{-# OPTIONS_GHC -Wno-type-defaults #-}

module Language.Logic.Test.Tagged where

import Language.Logic.Tagged

import Test.HUnit

tests :: Test
tests = TestLabel "Language.Logic.Test.Tagged" $
        TestList [testEq, testOrd, testShow, testStringId]

testEq :: Test
testEq = TestLabel "testEq" $ TestList [
    TestCase (assertBool "Equality test failed" $ Tagged "foo" 1 == Tagged "foo" 1),
    TestCase (assertBool "Equality test failed" $ Tagged "foo" 1 == Tagged "bar" 1),
    TestCase (assertBool "Equality test failed" $ Tagged "foo" 1 == Tagged "" 1),
    TestCase (assertBool "Equality test failed" $ Tagged "foo" 9 == Tagged "" 9),
    TestCase (assertBool "Equality test failed" $ Tagged (\_ -> 10) 9 == Tagged (\_ -> 20) 9), -- Non-Eq instance
    TestCase (assertBool "Equality test failed" $ Tagged "foo" 9 /= Tagged "" 10),
    TestCase (assertBool "Equality test failed" $ Tagged "foo" 9 /= Tagged "foo" 10)
  ]

testOrd :: Test
testOrd = TestLabel "testOrd" $ TestList [
    TestCase (assertBool "Ordering test failed" $ Tagged "foo" 20 > Tagged "foo" 10),
    TestCase (assertBool "Ordering test failed" $ Tagged "foo" 10 < Tagged "foo" 20),
    TestCase (assertBool "Ordering test failed" $ Tagged "foo" 20 > Tagged "bar" 10),
    TestCase (assertBool "Ordering test failed" $ Tagged "foo" 10 < Tagged "bar" 20),
    TestCase (assertBool "Ordering test failed" $ Tagged "bar" 20 > Tagged "foo" 10),
    TestCase (assertBool "Ordering test failed" $ Tagged "bar" 10 < Tagged "foo" 20),
    TestCase (assertBool "Ordering test failed" $ Tagged "bar" 20 > Tagged "bar" 10),
    TestCase (assertBool "Ordering test failed" $ Tagged "bar" 10 < Tagged "bar" 20),
    TestCase (assertBool "Ordering test failed" $ Tagged (\_ -> 10) 10 < Tagged (\_ -> 0) 20) -- Non-Ord instance
  ]

testShow :: Test
testShow = TestLabel "testShow" $ TestList [
    TestCase ("\"foo\"" @=? show (Tagged "foo" 100)),
    TestCase ("100" @=? show (Tagged 100 "foo")),
    TestCase ("100" @=? show (Tagged 100 (\_ -> ()))), -- Non-Show instance
    TestCase ("\"foo\"" @=? show (Tagged "foo" (\_ -> ()))) -- Non-Show instance
  ]

testStringId :: Test
testStringId = TestLabel "testStringId" $ TestList [
    TestCase (assertBool "Equality test failed" $ StringId "foo" == StringId "foo"),
    TestCase (assertBool "Equality test failed" $ StringId "foo" /= StringId "bar"),
    TestCase (assertBool "Ordering test failed" $ StringId "foo" <= StringId "foo"),
    TestCase (assertBool "Ordering test failed" $ StringId "bar" <= StringId "foo"),
    TestCase (assertBool "Ordering test failed" $ StringId "foo" >= StringId "foo"),
    TestCase (assertBool "Ordering test failed" $ StringId "foo" >= StringId "bar"),
    TestCase ("foo" @=? show (StringId "foo")),
    TestCase ("bar" @=? show (StringId "bar")),
    TestCase ("" @=? show (StringId "")),
    TestCase ("\"\"\"*'" @=? show (StringId "\"\"\"*'"))
  ]
