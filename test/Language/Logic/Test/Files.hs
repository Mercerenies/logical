
module Language.Logic.Test.Files where

import Language.Logic.Eval
import Language.Logic.Code
import Language.Logic.Parser
import Language.Logic.StdLib
import Language.Logic.Compile
import Language.Logic.SymbolTable
import Language.Logic.SymbolTable.Monad

import Test.HUnit
import Polysemy

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
            let sym = emptyTable
            contents <- readFile fpath
            (prelude, op, sym') <- getPrelude sym
            (clauses, _, sym'') <- eitherToIO (tokenizeAndParse op sym' fpath contents)
            let clauses' = consolidateClauses clauses
                (sym''', clauses'') = run $ runSymbolTableState sym'' (compileBody clauses')
                body = prelude <> clauses''
            (_, results) <- runProgram sym''' body >>= eitherToIO
            assertBool ("Test file " ++ fpath) $ (results > 0)

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
