
module Language.Logic.StdLib.Arithmetic(arithFunctions, evalArithWith, evalArith) where

import Language.Logic.Term
import Language.Logic.Number(Number(..))
import Language.Logic.Error

import Polysemy
import Polysemy.Error

import Data.Map(Map)
import qualified Data.Map as Map

type ArithFns = Map String ([Number] -> Either RuntimeError Number)

throwArith :: String -> Either RuntimeError a
throwArith = Left . ArithmeticError

_arg0 :: [Number] -> Either RuntimeError ()
_arg0 [] = pure ()
_arg0 _ = throwArith "Expecting zero arguments"

_arg1 :: [Number] -> Either RuntimeError Number
_arg1 [x] = pure x
_arg1 _ = throwArith "Expecting one argument"

_arg2 :: [Number] -> Either RuntimeError (Number, Number)
_arg2 [x, y] = pure (x, y)
_arg2 _ = throwArith "Expecting two arguments"

arg1or2 :: (Number -> Number) -> (Number -> Number -> Number) -> [Number] -> Either RuntimeError Number
arg1or2 f g xs = case xs of
                   [x] -> pure $ f x
                   [x, y] -> pure $ g x y
                   _ -> throwArith "Expecting one or two arguments"

arithFunctions :: ArithFns
arithFunctions = Map.fromList [
                  ("+", arg1or2 id (+)),
                  ("-", arg1or2 negate (-))
                 ]

evalArithWith :: Member (Error RuntimeError) r => ArithFns -> Term -> Sem r Number
evalArithWith _ (TermNum n) = pure n
evalArithWith _ (TermVar s) = throw (VarsNotDone [s])
evalArithWith fns (TermCompound h ts) =
    case Map.lookup h fns of
      Nothing -> throw $ ArithmeticError $ "No such function " ++ show h
      Just f -> mapM (evalArithWith fns) ts >>= fromEither . f

evalArith :: Member (Error RuntimeError) r => Term -> Sem r Number
evalArith = evalArithWith arithFunctions
