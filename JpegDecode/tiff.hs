module JpegDecoder.Tiff (
  parseTIFF
  ) where

import Numeric

import Data.Int
import Data.Word
import JpegDecoder.InternalDefs

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

import Data.Binary.ByteStreamDecoder as BSD

takeWord16beWithOffset :: Int -> BS.ByteString -> Word16
takeWord16beWithOffset offset str =
  (fromIntegral $ BSU.unsafeIndex str (offset+0)) * 256 +
  (fromIntegral $ BSU.unsafeIndex str (offset+1)) :: Word16

takeWord16be :: BS.ByteString -> Word16
takeWord16be = takeWord16beWithOffset 0

takeWord16le :: BS.ByteString -> Word16
takeWord16le str = (fromIntegral $ BSU.unsafeIndex str 1) * 256 +
                   (fromIntegral $ BSU.unsafeIndex str 0) :: Word16

takeWord32be :: BS.ByteString -> Word32
takeWord32be str = (fromIntegral $ BSU.unsafeIndex str 0) * 256 * 256 * 256 +
                   (fromIntegral $ BSU.unsafeIndex str 1) * 256 * 256       +
                   (fromIntegral $ BSU.unsafeIndex str 2) * 256             +
                   (fromIntegral $ BSU.unsafeIndex str 3) :: Word32

takeWord32le :: BS.ByteString -> Word32
takeWord32le str = (fromIntegral $ BSU.unsafeIndex str 3) * 256 * 256 * 256 +
                   (fromIntegral $ BSU.unsafeIndex str 2) * 256 * 256       +
                   (fromIntegral $ BSU.unsafeIndex str 1) * 256             +
                   (fromIntegral $ BSU.unsafeIndex str 0) :: Word32


parseTIFFle :: BS.ByteString -> ByteStreamDecoderT DecodeJpegCtx String IO ()
parseTIFFle exif = do
  let tag = takeWord16le exif
  liftIO $ putStrLn $ "tag=0x"++(showHex tag "")
  return ()

parseTIFFbe :: BS.ByteString -> ByteStreamDecoderT DecodeJpegCtx String IO ()
parseTIFFbe exif = throwError "need to implement parseTIFFbe"

parseTIFF :: BS.ByteString -> ByteStreamDecoderT DecodeJpegCtx String IO ()
parseTIFF exif = do
  let idstr  = BS.take 6 exif
      endianStr = BS.take 2 (BS.drop 6 exif)
      endian = takeWord16be endianStr
  liftIO $ putStrLn $ "EXIF"++",id="++(show idstr)++",byteorder=0x"++(showHex endian "")
  case endian of
    0x4949 -> parseTIFFle (BS.drop 6 exif)
    0x4D4D -> parseTIFFbe (BS.drop 6 exif)
    _      -> throwError "invalid value"
