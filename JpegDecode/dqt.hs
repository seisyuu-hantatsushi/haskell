{-# LANGUAGE CPP, OverloadedStrings #-}

module JpegDecoder.Dqt (
  parseDQT
  ) where

import Data.Bits
import Data.Int
import Data.Word
import qualified Data.Vector as V

import Data.Binary.ByteStreamDecoder as BSD

import Data.Maybe
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

import JpegDecoder.InternalDefs

appendDQTIter :: BS.ByteString -> DQT -> DQT
appendDQTIter dqtsStr dqt
  | BS.length dqtsStr == 0  = dqt
  | BS.length dqtsStr == 65 =
    let
      pq = (fromIntegral $ (BS.head dqtsStr) .&. 0xF0 :: Word8) `unsafeShiftR` 4 
      tq = fromIntegral $ (BS.head dqtsStr) .&. 0x0F :: Word8 
    in
     appendDQTIter (BS.drop 1 dqtsStr) (DQT pq tq (dqtQs dqt))
  | otherwise =
      let
        newDQT =
          let
            newQS = V.snoc (dqtQs dqt) (fromIntegral $ BS.head dqtsStr :: Word8)
          in
           DQT (dqtPq dqt) (dqtTq dqt) newQS
      in 
       appendDQTIter (BS.drop 1 dqtsStr) newDQT 
        
appendDQT :: BS.ByteString -> ByteStreamDecoderT DecodeJpegCtx String IO ()
appendDQT dqtsStr
  | (BS.length dqtsStr) `mod` 65 /= 0 =
    throwError "invalid DQT length"
  | (BS.length dqtsStr) > 65 =
    throwError "need to implement, more than 2 DQT in frame."
  | otherwise = --本当はさらに(BS.length dqtsStr)を65で割った回数でLoop
    (do
        let newDQT = appendDQTIter dqtsStr (DQT 0 0 V.empty)  
        ctx <- getContext
        putContext $ updateDecodeJpegCtx ctx (CtxDqts $ V.snoc (dqts ctx) newDQT)
        liftIO $ putStrLn $ show newDQT
        return ()
    )

parseDQT :: (DecodeJpegState -> ByteStreamDecoderT DecodeJpegCtx String IO ()) -> ByteStreamDecoderT DecodeJpegCtx String IO ()
parseDQT decodeJpegIter = do
  len <- getWord16be
  len <- checkValid len
  remain <- BSD.length
  liftIO $ putStrLn $ "parseDQT: len=" ++ (show len)
  -- lenは 2+65*tである
  -- t = (len-2)/65
  if (remain < fromIntegral (len - 2))
    then throwError "eof (too short)"
    else (do
             dqtsStr <- readN $ fromIntegral (len - 2)
             appendDQT dqtsStr
             ctx <- getContext
             decodeJpegIter (state ctx))
