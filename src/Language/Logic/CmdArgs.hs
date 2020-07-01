
module Language.Logic.CmdArgs where

import Options.Applicative

import Language.Logic.Debug
import qualified Language.Logic.Util as Util

data CmdArgs = CmdArgs {
      cmdDebugLevel :: DebugLevel,
      cmdDebugKB :: Bool,
      cmdFileName :: FilePath
    }

readDebugLevel :: ReadM DebugLevel
readDebugLevel = auto >>= Util.hoistMaybe . intToDebug

optDebugLevel :: Parser DebugLevel
optDebugLevel = option readDebugLevel
                (  long "debug-level"
                <> help "How much debug output to display"
                <> showDefaultWith (show . fromEnum)
                <> value NoDebug)

optDebugKB :: Parser Bool
optDebugKB = switch
              (  long "debug-print-kb"
              <> help "Display the knowledge base before executing")

optFileName :: Parser FilePath
optFileName = argument str (metavar "FILE")

cmdArgsParser :: Parser CmdArgs
cmdArgsParser = CmdArgs <$> optDebugLevel <*> optDebugKB <*> optFileName

cmdArgsDesc :: InfoMod CmdArgs
cmdArgsDesc = fullDesc <> header "A logic-oriented programming language"

cmdArgsInfo :: ParserInfo CmdArgs
cmdArgsInfo = info (cmdArgsParser <**> helper) cmdArgsDesc

parseCmd :: IO CmdArgs
parseCmd = execParser cmdArgsInfo
