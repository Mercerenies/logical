
module Language.Logic.Test.Unify where

import Language.Logic.Term
import Language.Logic.Unify

import Test.HUnit
import Polysemy
import Polysemy.Error

import Data.Function
import qualified Data.Map as Map

tests :: Test
tests = TestLabel "Language.Logic.Test.Unify" $ TestList [testEqual, testSimpleVar, testDeepVar]

unifyTestPass :: Term -> Term -> Term -> [(String, Term)] -> Test
unifyTestPass s t u0 asm0 = TestCase $
    case fullUnify s t & runError & run of
      Left err -> assertFailure $ prefix ++ ": " ++ show err
      Right (asm, u) -> assertEqual prefix u0 u >> assertEqual prefix (Assumptions $ Map.fromList asm0) asm
    where prefix = "Unification of " ++ show s ++ " and " ++ show t

unifyTestFail :: Term -> Term -> Test
unifyTestFail s t = TestCase $
    case fullUnify s t & runError & run of
      Left _ -> pure ()
      Right (_, u) -> assertFailure $ prefix ++ ": Expecting failure, got " ++ show u
    where prefix = "Unification of " ++ show s ++ " and " ++ show t

testEqual :: Test
testEqual = TestLabel "testEqual" $ TestList [
             unifyTestPass (TermInt 1) (TermInt 1) (TermInt 1) [],
             unifyTestPass (TermAtom "a") (TermAtom "a") (TermAtom "a") [],
             let t = TermCompound "foo" [TermAtom "a", TermCompound "b" []] in unifyTestPass t t t [],
             unifyTestFail (TermInt 1) (TermInt 3),
             unifyTestFail (TermAtom "a") (TermAtom "b"),
             unifyTestFail (TermAtom "a") (TermInt 3),
             let t = TermCompound "foo" [TermAtom "a", TermCompound "b" []] in unifyTestFail t (TermInt 10),
             let t x = TermCompound "foo" [TermAtom "a", TermInt x] in unifyTestFail (t 10) (t 11)
            ]

testSimpleVar :: Test
testSimpleVar = TestLabel "testSimpleVar" $ TestList [
                 unifyTestPass (TermVar "X") (TermInt 99) (TermInt 99) [("X", TermInt 99)],
                 unifyTestPass (TermInt 99) (TermVar "X") (TermInt 99) [("X", TermInt 99)],
                 unifyTestPass (TermVar "X") (TermVar "Y") (TermVar "Y") [("X", TermVar "Y")],
                 unifyTestPass (TermVar "Y") (TermVar "X") (TermVar "Y") [("X", TermVar "Y")],
                 let t x = TermCompound "foo" [TermVar x] in
                 unifyTestPass (t "X") (t "Y") (t "Y") [("X", TermVar "Y")],
                 unifyTestFail (TermVar "X") (TermCompound "foo" [TermVar "X"]),
                 unifyTestFail (TermCompound "pair" [TermVar "X", TermVar "Y"])
                               (TermCompound "pair" [TermCompound "foo" [TermVar "Y"],
                                                     TermCompound "foo" [TermVar "X"]])
                ]

testDeepVar :: Test
testDeepVar = TestLabel "testDeepVar" $ TestList [
               let t = TermCompound "foo" [TermVar "Y"] in unifyTestPass (TermVar "X") t t [("X", t)],
               let t x = TermCompound "foo" [x] in unifyTestPass (t (TermVar "X")) (t (t (TermVar "Y")))
                                                                 (t (t (TermVar "Y"))) [("X", t (TermVar "Y"))]
              ]
