{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      SBP2JSON
-- Copyright:   Copyright (C) 2015 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Mark Fine <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- SBP to JSON tool - reads SBP binary from stdin and sends SBP JSON
-- to stdout.

import           BasicPrelude
import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.Encode
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8             as BC8
import qualified Data.ByteString.Lazy              as BL
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List                 as CL
import           Data.Conduit.Network
import           Data.Conduit.Serialization.Binary
import           Data.Serialize
import           Data.Serialize.Put
import           Network.MQTT
import           Network.MQTT.Types
import           SwiftNav.SBP
import           System.Environment
import           System.IO

getGPSOption :: SBPMsg -> Maybe (Either MsgImuRaw MsgPosLlh)
getGPSOption (SBPMsgPosLlh m _) = Just $ Right m
getGPSOption (SBPMsgImuRaw m _) = Just $ Left m
getGPSOption _                  = Nothing

publishLlh :: Config -> MsgPosLlh -> IO ()
publishLlh conf msg = do
  publish conf NoConfirm False "gps" $ runPut $ do
    put $ msg ^. msgPosLlh_lon
    put $ msg ^. msgPosLlh_lat
    put $ msg ^. msgPosLlh_height
    put $ msg ^. msgPosLlh_n_sats

publishImu :: Config -> MsgImuRaw -> IO ()
publishImu conf msg = do
  publish conf NoConfirm False "orientation" $ runPut $ do
    put $ roll
    put $ pitch
  where
    roll = (atan2 y z) * 180 / pi
    pitch = (atan2 (negate x) (sqrt (y*y + z*z))) * 180 / pi
    x,y,z :: Double
    x = fromIntegral $ _msgImuRaw_acc_x msg
    y = fromIntegral $ _msgImuRaw_acc_y msg
    z = fromIntegral $ _msgImuRaw_acc_z msg

gpsWill :: Will
gpsWill =
  Will
    True -- Retain
    Handshake
    "system_down"
    "rover-gps"

main :: IO ()
main = do
  gpsHost <- getEnv "SORO_GPS_SERVER_IP"
  mqttHost <- getEnv "SORO_MQTT_BROKER_IP"
  cmds <- mkCommands
  pubChan <- newTChanIO
  let
    conf =
      (defaultConfig cmds pubChan) {
          cWill = Just gpsWill
        , cHost = mqttHost
        }
  forkIO $ forever $ do
    run conf
    threadDelay $ 10 ^ 6

  runTCPClient (clientSettings 55555 $ BC8.pack gpsHost) $ \ad ->
    appSource ad  =$=
      conduitDecode     =$=
      CL.map getGPSOption =$=
      CL.catMaybes $$
      CL.mapM_ (either (publishImu conf) (publishLlh conf))
