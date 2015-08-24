{-# LANGUAGE OverloadedStrings, RecordWildCards #-} 

module Main where
import Network.IRC.Client
import Network.IRC.Client.Types

main = do
  let host = "chat.freenode.net" 
  let port = 6667
  let nick = "dan"
  conn <- connect host port 1
  let cfg = defaultIRCConf nick
  -- let cfg' = cfg { _eventHandlers = handler : _eventHandlers cfg }
  -- let cfg' = cfg { _eventHandlers = handler : _eventHandlers cfg }
  start conn cfg




handler :: EventHandler
handler = undefined

{-
run :: ByteString -> Int -> Text -> IO ()
run host port nick = do
  conn <- connect host port 1
  let cfg = defaultIRCConf nick
  let cfg' = cfg { _handlers = yourCustomEventHandlers : _handlers cfg }
  start conn cfg'
  -}
