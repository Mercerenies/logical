
module Language.Logic.Test.Term where

import Language.Logic.Term

import Test.HUnit

--import Data.Set(Set)
import qualified Data.Set as Set

tests :: Test
tests = TestLabel "Language.Logic.Test.Term" $ TestList [testAccessors, testFree, testFreeFact,
                                                         testSafeVar, testRenameVars]

-- I just want to know if the elements are the same. I don't care if
-- they get produced in the same order.
assertEqNoOrd :: (Ord a, Show a) => String -> [a] -> [a] -> IO ()
assertEqNoOrd prefix exp_ act = assertEqual prefix (Set.fromList exp_) (Set.fromList act)

testAccessors :: Test
testAccessors = TestLabel "testAccessors" $ TestList [
    TestCase ("foo" @=? factHead (Fact "foo" [])),
    TestCase ("a0" @=? factHead (Fact "a0" [])),
    TestCase ([] @=? factBody (Fact "foo" [])),
    TestCase ([TermNum 999] @=? factBody (Fact "foo" [TermNum 999]))
  ]

testFree :: Test
testFree = TestLabel "testFree" $ TestList [
    TestCase (assertEqNoOrd "" [] (freeVars (TermNum 1))),
    TestCase (assertEqNoOrd "" ["ABC"] (freeVars (TermVar "ABC"))),
    TestCase (assertEqNoOrd "" [] (freeVars (TermCompound "foo" []))),
    TestCase (assertEqNoOrd "" ["A", "B", "C"]
                               (freeVars (TermCompound "foo" [TermVar "A", TermVar "B", TermVar "C"]))),
    TestCase (assertEqNoOrd "" ["A", "B", "C"]
                               (freeVars (TermCompound "foo" [TermVar "A", TermVar "B", TermNum 0, TermVar "C"]))),
    TestCase (assertEqNoOrd "" ["Foo", "Bar"]
                               (freeVars (TermCompound "foo" [TermCompound "bar" [TermVar "Bar", TermNum 0],
                                                              TermVar "Foo"])))
  ]

testFreeFact :: Test
testFreeFact = TestLabel "testFreeFact" $ TestList [
    TestCase (assertEqNoOrd "" [] (freeVarsInFact (Fact "fact" []))),
    TestCase (assertEqNoOrd "" ["AA", "BB"] (freeVarsInFact (Fact "fact" [TermVar "AA", TermVar "BB"]))),
    TestCase (assertEqNoOrd "" [] (freeVarsInFact (Fact "fact" [TermNum 9]))),
    TestCase (assertEqNoOrd "" ["C"]
                               (freeVarsInFact (Fact "fact" [TermCompound "a" [TermCompound "b" [TermVar "C"]]])))
  ]

testSafeVar :: Test
testSafeVar = TestLabel "testSafeVar" $ TestList [
    TestCase ("A" @=? safeVar [] "A"),
    TestCase ("A" @=? safeVar ["B", "C", "D"] "A"),
    TestCase ("A0" @=? safeVar ["A"] "A"),
    TestCase ("A1" @=? safeVar ["A", "A0"] "A"),
    TestCase ("A2" @=? safeVar ["A", "B", "A0", "B0", "A1"] "A"),
    TestCase ("A10" @=? safeVar ["A", "B", "A0", "B0", "A1"] "A1")
  ]

testRenameVars :: Test
testRenameVars = TestLabel "testRenameVars" $ TestList [
    TestCase (TermNum 9 @=? renameVars [] (TermNum 9)),
    TestCase (TermCompound "foo" [] @=? renameVars [] (TermCompound "foo" [])),
    TestCase (TermNum 9 @=? renameVars ["A", "B"] (TermNum 9)),
    TestCase (TermCompound "foo" [] @=? renameVars ["A", "B"] (TermCompound "foo" [])),
    TestCase (TermVar "A0" @=? renameVars ["A"] (TermVar "A")),
    TestCase ((TermCompound "foo" [TermVar "A0", TermVar "B"]) @=?
              renameVars ["A"] (TermCompound "foo" [TermVar "A", TermVar "B"])),
    TestCase ((TermCompound "foo" [TermVar "A1", TermVar "A0"]) @=?
              renameVars ["A"] (TermCompound "foo" [TermVar "A", TermVar "A0"])),
    TestCase ((TermCompound "foo" [TermVar "A0", TermVar "A0"]) @=?
              renameVars ["A"] (TermCompound "foo" [TermVar "A", TermVar "A"]))
  ]
