module Main where

import qualified Data.ByteString.Char8 as B
import System.Hardware.Serialport
import System.IO
import Control.Exception
import Control.Monad

main :: IO ()
main = do
  let port = "/dev/ttyUSB0"  -- Linux
  bracket
    (hOpenSerial port defaultSerialSettings { commSpeed = CS115200 , timeout = 10})
    hClose 
    $ \h -> do
      forever $ do
        B.hGetLine h >>= print
