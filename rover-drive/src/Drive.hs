{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Drive where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad
import           Data.ByteString hiding (putStrLn)
import qualified Data.Serialize           as S
import           Data.Word
import           Network.MQTT
import           System.Environment

addSubscriptions :: Config -> Commands -> [Topic] -> IO ()
addSubscriptions conf cmds subscriptions = do
  subscribe conf $ (,NoConfirm) <$> subscriptions
  return ()

getTopic :: ByteString -> Topic
getTopic bs =
  case S.decode bs :: Either String Word8 of
    Left err -> "drive/controller"
    Right 0  -> "drive/controller"
    Right 1  -> "drive/autonomous"

driveSelector :: Config -> Commands -> IO (TVar Topic)
driveSelector conf cmds = do
  selectedTopic <- newTVarIO "drive/controller"
  selectedTopicChan <- newTChanIO
  -- Start a thread to constantly
  -- watch for changes in the selected topic
  -- and then set the selected topic
  async $ forever $ atomically $ do
    msg <- readTChan selectedTopicChan
    let
      t = getTopic . payload $ body msg
    writeTVar selectedTopic t
  subscribe conf [("drive_switch", Handshake)]
  return selectedTopic

startDriveMux :: String -> IO ()
startDriveMux mqttHost = do
  putStrLn "Making commands"
  cmds <- mkCommands
  putStrLn "Making dummy channel"
  pubChan <- newTChanIO
  let
    conf = (defaultConfig cmds pubChan) { cHost = mqttHost }
  _ <- forkIO $ forever $ run conf
  putStrLn "Adding subscriptions"
  addSubscriptions conf cmds ["drive/controller", "drive/autonomous"]
  putStrLn "starting drive selector"
  selectedTopic <- driveSelector conf cmds

  putStrLn "Starting async"
  async $ forever $ do
    (msg, top) <- atomically $ do
      m <- readTChan pubChan
      t <- readTVar selectedTopic
      return (m,t)
    print ("Received: ", payload $ body msg, topic (body msg))
    when (topic (body msg) `matches` top) $ do
      print ("Publishing: ", payload $ body msg)
      publish conf NoConfirm False "drive" $ payload $ body msg
  return ()


