module Main where

import Drive
import System.Environment
import Control.Concurrent

main :: IO ()
main = do
  mqttHost <- getEnv "SORO_MQTT_BROKER_IP"
  startDriveMux mqttHost
  threadDelay maxBound