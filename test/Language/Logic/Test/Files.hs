
module Language.Logic.Test.Files where

import Language.Logic.Eval
import Language.Logic.Code
import Language.Logic.Parser
import Language.Logic.StdLib

import Test.HUnit

import Data.Char
import System.Directory

eitherToIO :: Show e => Either e a -> IO a
eitherToIO (Left e) = fail (show e)
eitherToIO (Right x) = pure x

endsWith :: Eq a => [a] -> [a] -> Bool
endsWith whole ext = drop (length whole - length ext) whole == ext

runTestFile :: FilePath -> Test
runTestFile fpath = TestLabel fpath $ TestCase go
    where go = do
            contents <- readFile fpath
            clauses <- eitherToIO (tokenizeAndParse fpath contents)
            prelude <- getPrelude
            let body = prelude <> consolidateClauses clauses
            runProgram body >>= eitherToIO

discoverTestFiles :: FilePath -> IO [FilePath]
discoverTestFiles fpath =
    doesFileExist fpath >>= \case
      False -> listDirectory fpath >>= \fpaths ->
               concat <$> mapM (\p -> discoverTestFiles (fpath ++ "/" ++ p)) fpaths
      True -> pure (if endsWith (fmap toLower fpath) ".txt" then [fpath] else [])

allTestFiles :: IO Test
allTestFiles = do
  allfiles <- discoverTestFiles "std/test"
  let tests = fmap runTestFile allfiles
  pure $ TestLabel "Language.Logic.Test.Files" (TestList tests)
