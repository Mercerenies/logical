{-# LANGUAGE OverloadedStrings #-}

module Language.Logic.StdLib.String(stringConcat, stringLength) where

import Language.Logic.StdLib.Util
import Language.Logic.Term.Compiled
import qualified Language.Logic.Util as Util

import qualified Data.Text as T
import Polysemy

import Control.Monad

removePrefix :: T.Text -> T.Text -> Maybe T.Text
removePrefix needle haystack
    | T.isPrefixOf needle haystack = Just $ T.drop (T.length needle) haystack
    | otherwise = Nothing

removeSuffix :: T.Text -> T.Text -> Maybe T.Text
removeSuffix needle haystack
    | T.isSuffixOf needle haystack = Just $ T.dropEnd (T.length needle) haystack
    | otherwise = Nothing

allSplits :: T.Text -> [(T.Text, T.Text)]
allSplits t = fmap (\n -> T.splitAt n t) [0..T.length t]

stringConcat :: EvalCtx' r => CFact -> Sem r ()
stringConcat = arg3 >=> \case
               (CTermString a, CTermString b, c) ->
                   unify (CTermString $ a <> b) c
               (CTermString a, b, CTermString c) ->
                   Util.hoistMaybe (removePrefix a c) >>= \s -> unify (CTermString s) b
               (a, CTermString b, CTermString c) ->
                   Util.hoistMaybe (removeSuffix b c) >>= \s -> unify (CTermString s) a
               (a, b, CTermString c) ->
                   Util.oneOf $ fmap (\(a', b') -> unify (CTermString a') a >> unify (CTermString b') b) (allSplits c)
               (a, b, c) -> expectingError valid "variable or string" [a, b, c]
    where valid (CTermVar _) = True
          valid (CTermString _) = True
          valid _ = False

stringLength :: EvalCtx' r => CFact -> Sem r ()
stringLength = arg2 >=> \(s, n) -> assertString s >>=
               \s' -> unify (CTermNum $ fromIntegral (T.length s')) n
