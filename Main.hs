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

data Options = Options {
        nickname :: Text 
      , chatroom :: Text 
      } deriving Show

parseOpts :: Parser Options
parseOpts = Options   
        <$> (T.pack <$> strArgument (metavar "NICK" <> help "Nickname"))
        <*> (prependHash . T.pack <$> strArgument (metavar "CHANNEL" <> help "Channel, # is automatically prepended if missing"))

opts = info (helper <*> parseOpts)
         (fullDesc <> header "irclog")

main = do
  opt@Options{..} <- execParser opts
  print opt
  hSetBuffering stdout NoBuffering
  let host = "chat.freenode.net" 
  let port = 6667
  -- noopLogger, stdoutLogger
  conn <- connect' noopLogger host port 1
  let cfg = defaultIRCConf nickname
  let hs = [joinChatHandler chatroom, messageHandler]
  -- let hs = [joinChatHandler chatroom, logEventsHandler]
  let cfg' = cfg { _eventHandlers = hs ++ _eventHandlers cfg }
  start conn cfg'

prependHash :: Text -> Text
prependHash s | T.isPrefixOf "#" s = s
              | otherwise = T.cons '#' s


logEventsHandler :: EventHandler
logEventsHandler = EventHandler "Trace chat events" EEverything logEvent 

logEvent :: UnicodeEvent -> IRC ()
logEvent ev = do
    liftIO . print $ ev
    liftIO . print . eventType $ ev

messageHandler :: EventHandler
messageHandler = EventHandler "Log messages" EPrivmsg logMessage


-- default event handlers
-- https://hackage.haskell.org/package/irc-client-0.2.4.0/docs/src/Network-IRC-Client-Handlers.html#defaultEventHandlers

joinChatHandler ch = EventHandler "Join chatroom" ENumeric (joinChat ch)

joinChat :: Text -> (UnicodeEvent -> IRC ())
joinChat ch ev = 
  case _message ev of
    Numeric 001 _ -> send (Join ch)
    _ -> return ()

logMessage :: UnicodeEvent -> IRC ()
logMessage ev = 
  case _message ev of
    Privmsg target m -> liftIO $ do
        now <- getCurrentTime
        let ts =  formatTime defaultTimeLocale "%c" now

        let source = T.unpack $ formatSource (_source ev)
        printf "%s %20s : " ts source 
        -- print target
        T.putStrLn (formatMsg m)

    _ -> return ()
    
formatSource :: Source Text -> Text
formatSource (Channel name nick) = nick
formatSource (User nick) = nick
formatSource (Server n) = n

formatMsg :: Either b Text -> Text
formatMsg (Right m) = m
formatMsg (Left e) = "CTCPByteString"
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

-}
