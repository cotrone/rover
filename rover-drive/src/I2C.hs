{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module I2C where

import Foreign.C.Types
import Data.Word
import Data.Bits

-- include <linux/i2c.h>
-- foreign import ccall unsafe "i2c i2c_smbus_write_quick" i2c_smbus_write_quick_c  :: CInt -> CUChar -> IO CInt
-- foreign import ccall unsafe "i2c_smbus_read_byte" i2c_smbus_read_byte_c :: CInt -> IO CInt
foreign import ccall unsafe "i2c i2c_smbus_write_byte" i2c_smbus_write_byte_c :: CInt -> CUChar -> IO CInt -- (int file, __u8 value)
-- foreign import ccall unsafe "i2c_smbus_read_byte_data" i2c_smbus_read_byte_data_c :: CInt -> CUChar -> IO CInt --(int file, __u8 command)
foreign import ccall unsafe "i2c i2c_smbus_write_byte_data" i2c_smbus_write_byte_data_c :: CInt -> CUChar -> CUChar -> IO CInt -- (int file, __u8 command, __u8 value)
-- foreign import ccall unsafe "i2c_smbus_read_word_data" i2c_smbus_read_word_data_c :: CInt -> CUChar -> IO CInt -- (int file, __u8 command)
-- foreign import ccall unsafe "i2c_smbus_write_word_data" i2c_smbus_write_word_data_c :: CInt -> CUChar -> CUShort -> IO CInt --(int file, __u8 command, __u16 value)
-- foreign import ccall unsafe "i2c_smbus_process_call" i2c_smbus_process_call_c :: CInt -> CUChar -> CUShort -> IO CInt -- (int file, __u8 command, __u16 value)
-- foreign import ccall unsafe "i2c_smbus_read_block_data" i2c_smbus_read_block_data_c :: (int file, __u8 command, __u8 *values)
-- foreign import ccall unsafe "i2c_smbus_write_block_data" i2c_smbus_write_block_data_c :: (int file, __u8 command, __u8 length, __u8 *values)
-- foreign import ccall unsafe "i2c_smbus_read_i2c_block_data" i2c_smbus_read_i2c_block_data_c :: (int file, __u8 command, __u8 *values)
-- foreign import ccall unsafe "i2c_smbus_write_i2c_block_data" i2c_smbus_write_i2c_block_data_c :: (int file, __u8 command, __u8 length, __u8 *values)
-- foreign import ccall unsafe "i2c_smbus_block_process_call" i2c_smbus_block_process_call_c :: (int file, __u8 command, __u8 length, __u8 *values)
-- include <linux/i2c-dev.h>

newtype I2C = I2C { unI2C :: CInt } deriving (Eq, Show)

writeByte :: I2C -> Word8 -> IO Int
writeByte (I2C fh) byte = fromIntegral <$> i2c_smbus_write_byte_c fh (CUChar byte)

writeByteData :: I2C -> Word8 -> Word8 -> IO Int
writeByteData (I2C fh) byte d = fromIntegral <$> i2c_smbus_write_byte_data_c fh (CUChar byte) (CUChar d)

setFrequency :: I2C -> Int -> IO Int
setFrequency i2c freq = do
  write mode1 0x10
  write preScale prescaleVal
  write mode1 0x80
  write mode2 0x04
  where
    prescaleVal :: Word8
    prescaleVal = round prescaleValDouble
    prescaleValDouble :: Double
    prescaleValDouble = (clockFreq / 4096 / (fromIntegral freq)) - 1
    write = writeByteData i2c
    mode1 = 0x00
    mode2 = 0x01
    preScale = 0xFE
    clockFreq = 25000000.0

setPwm :: I2C -> Int -> Int -> Int -> IO Int
setPwm i2c pin on off = do
  write (ledOnL + mult) (fromIntegral $ on' .&. 0xFF)
  write (ledOnH + mult) (fromIntegral $ shiftR on' 8)
  write (ledOffL + mult) (fromIntegral $ off' .&. 0xFF)
  write (ledOffH + mult) (fromIntegral $ shiftR off' 8)
  where
    off' :: Word16
    off' = fromIntegral off
    on' :: Word16
    on' = fromIntegral on
    pin' = fromIntegral pin
    write = writeByteData i2c
    ledOnL = 0x6
    ledOnH = 0x7
    ledOffL = 0x8
    ledOffH = 0x9
    ledMultiplier = 4
    mult = ledMultiplier * (pin' - 1)