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

addSubscriptions :: Commands -> [Topic] -> IO (TChan (Message PUBLISH))
addSubscriptions cmds subscriptions = do
  chan <- newTChanIO
  let
    conf = defaultConfig cmds chan
  subscribe conf $ (,NoConfirm) <$> subscriptions
  return chan

getTopic :: ByteString -> Topic
getTopic bs =
  case S.decode bs :: Either String Word8 of
    Left err -> "drive/controller"
    Right 0  -> "drive/controller"
    Right 1  -> "drive/autonomous"

driveSelector :: Commands -> IO (TVar Topic)
driveSelector cmds = do
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
  let
    conf = defaultConfig cmds selectedTopicChan
  subscribe conf [("drive_switch", Handshake)]
  return selectedTopic

startDriveMux :: String -> IO ()
startDriveMux mqttHost = do
  putStrLn "Making commands"
  cmds <- mkCommands
  putStrLn "Making dummy channel"
  stupidChan <- newTChanIO
  putStrLn "Adding subscriptions"
  chan <- addSubscriptions cmds ["drive/controller", "drive/autonomous"]
  putStrLn "starting drive selector"
  selectedTopic <- driveSelector cmds
  let
    conf = (defaultConfig cmds stupidChan) { cHost = mqttHost }
  putStrLn "Starting async"
  async $ forever $ do
    (msg, top) <- atomically $ do
      m <- readTChan chan
      t <- readTVar selectedTopic
      return (m,t)
    when (topic (body msg) `matches` top) $ do
      publish conf NoConfirm False "drive" $ payload $ body msg
  return ()


