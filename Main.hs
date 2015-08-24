{-# LANGUAGE OverloadedStrings, RecordWildCards #-} 

module Main where
import Network.IRC.Client
import Network.IRC.Client.Types
import System.Environment
import Options.Applicative 
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class

data Options = Options {
        nickname :: Text 
      , chatroom :: Text 
      } deriving Show

parseOpts :: Parser Options
parseOpts = Options   
        <$> (T.pack <$> strArgument (metavar "NICK" <> help "nickname"))
        <*> (T.pack <$> strArgument (metavar "CHANNEL"))

opts = info (helper <*> parseOpts)
         (fullDesc <> header "bhirc")

main = do
  opt@Options{..} <- execParser opts
  print opt
  let host = "chat.freenode.net" 
  let port = 6667
  conn <- connect' stdoutLogger host port 1
  let cfg = defaultIRCConf nickname
  let cfg' = cfg { _eventHandlers = [joinChatHandler chatroom, logEventsHandler] ++ _eventHandlers cfg }
  start conn cfg'

logEventsHandler :: EventHandler
logEventsHandler = EventHandler "Trace chat events" EEverything logEvent 

logEvent :: UnicodeEvent -> IRC ()
logEvent ev = do
    liftIO . print $ ev
    liftIO . print . eventType $ ev

-- default event handlers
-- https://hackage.haskell.org/package/irc-client-0.2.4.0/docs/src/Network-IRC-Client-Handlers.html#defaultEventHandlers

joinChatHandler ch = EventHandler "Join chatroom" ENumeric (joinChat ch)

joinChat :: Text -> (UnicodeEvent -> IRC ())
joinChat ch ev = 
  case _message ev of
    Numeric 001 _ -> send (Join ch)
    _ -> return ()



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
-}
