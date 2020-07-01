
module Language.Logic.Debug where

import Colog.Core
import Colog.Polysemy
import Polysemy

import Prelude hiding (log)

data Message = Message {
      msgDebugLevel :: DebugLevel,
      msgText :: String
    } deriving (Eq)

data DebugLevel = NoDebug | MinDebug | SomeDebug | MaxDebug
                  deriving (Show, Read, Eq, Ord, Enum)

intToDebug :: Int -> Maybe DebugLevel
intToDebug 0 = Just NoDebug
intToDebug 1 = Just MinDebug
intToDebug 2 = Just SomeDebug
intToDebug 3 = Just MaxDebug
intToDebug _ = Nothing

cfilterDebugLevel :: Applicative m => DebugLevel -> LogAction m Message -> LogAction m Message
cfilterDebugLevel dbg = cfilter (\msg -> dbg >= msgDebugLevel msg)

logMsg :: Member (Log Message) r => DebugLevel -> String -> Sem r ()
logMsg dbg msg = log (Message dbg msg)

logMsgMin :: Member (Log Message) r => String -> Sem r ()
logMsgMin = logMsg MinDebug

logMsgSome :: Member (Log Message) r => String -> Sem r ()
logMsgSome = logMsg SomeDebug

logMsgMax :: Member (Log Message) r => String -> Sem r ()
logMsgMax = logMsg MaxDebug

logAction :: DebugLevel -> LogAction IO Message
logAction dbg = cfilterDebugLevel dbg . cmap msgText $ logStringStderr

runLogMsg :: Member (Embed IO) r => DebugLevel -> Sem (Log Message ': r) a -> Sem r a
runLogMsg dbg = runLogAction (logAction dbg)
