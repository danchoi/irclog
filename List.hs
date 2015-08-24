{-# LANGUAGE OverloadedStrings, RecordWildCards #-} 

module Main where
import Network.IRC.Client
import Network.IRC.Client.Types
import System.Environment
import Options.Applicative 
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.IO.Class
import System.IO 
import Data.Time.Clock            (NominalDiffTime, getCurrentTime)
import Data.Time.Format           (formatTime)
import System.Locale    (defaultTimeLocale)
import Text.Printf


main = do
  hSetBuffering stdout NoBuffering
  let host = "chat.freenode.net" 
  let port = 6667
  -- noopLogger, stdoutLogger
  conn <- connect' noopLogger host port 1
  let cfg = defaultIRCConf "testawlkjawd"
  let hs = [logEventsHandler, listChannelsHandler]
  -- let hs = [listChannelsHandler chatroom, logEventsHandler]
  let cfg' = cfg { _eventHandlers = hs ++ _eventHandlers cfg }
  start conn cfg'

-- https://hackage.haskell.org/package/irc-client-0.2.4.0/docs/src/Network-IRC-Client-Handlers.html#defaultEventHandlers

listChannelsHandler = EventHandler "Join chatroom" ENumeric listChans 

listChans :: (UnicodeEvent -> IRC ())
listChans ev = 
  case _message ev of
    Numeric 001 _ -> send (RawMsg "LIST")
    _ -> return ()

logEventsHandler :: EventHandler
logEventsHandler = EventHandler "Trace chat events" EEverything logServerMessage

logEvent :: UnicodeEvent -> IRC ()
logEvent ev = do
    liftIO . print $ ev
    liftIO . print . eventType $ ev

-- list item looks like:
-- Event {_raw = ":leguin.freenode.net 322 testawlkjawd #inspirehep 3 :INSPIRE North America.", _source = Server "leguin.freenode.net", _message = Numeric 322 ["testawlkjawd","#inspirehep","3","INSPIRE North America."]}


logServerMessage :: UnicodeEvent -> IRC ()
logServerMessage ev = 
  case _message ev of 
    -- Numeric 322 xs@[target, chan, num, info] -> liftIO $ do
    Numeric 322 xs@[target, chan, num, info] -> liftIO $ do
        T.putStrLn $ T.unwords [chan, num, info] 
    _ -> return ()
        
        


{-
Channel "#haskell" "Welkin"
"#haskell"
Right "I know"
---
Channel "#haskell" "quchen"
"#haskell"
Right "But your deps lack that."
---

-}

{-
send :: UnicodeMessage -> IRC ()

type EventHandler Text EventType (UnicodeEvent -> IRC ())


handler :: Text -> EventHandler
handler chatroom = undefined -- EventHandler "Join chatroom at beginning" 

run :: ByteString -> Int -> Text -> IO ()
run host port nick = do
  conn <- connect host port 1
  let cfg = defaultIRCConf nick
  let cfg' = cfg { _handlers = yourCustomEventHandlers : _handlers cfg }
  start conn cfg'


Messages

  http://hackage.haskell.org/package/irc-conduit-0.1.2.0/docs/src/Network-IRC-Conduit-Internal-Messages.html#Message

  https://en.wikipedia.org/wiki/List_of_Internet_Relay_Chat_commands#LIST

-}
