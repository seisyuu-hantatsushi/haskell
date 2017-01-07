{-# LANGUAGE CPP, OverloadedStrings #-}

module JpegDecoder (
  runJpegDecoder
  ) where

import System.IO
import System.Environment

import Numeric

import Data.Bits
import Data.Int
import Data.Word
import Data.Array

import qualified Data.Vector as V

import Data.Binary.ByteStreamDecoder as BSD

import Data.Maybe
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

import JpegDecoder.InternalDefs
import JpegDecoder.Tiff
import JpegDecoder.Dqt
import JpegDecoder.Dht
import JpegDecoder.StartOfScan
import JpegDecoder.DecodeEntropy

searchSOI :: ByteStreamDecoderT DecodeJpegCtx String IO ()
searchSOI = do
  c0  <- getWord8
  c0' <- checkValid c0
  c1  <- peekWord8
  c1' <- checkValid c1
  if (c0' == 0xFF) &&  (c1' == 0xD8)
    then (do
             BSD.drop 1
             ctx <- getContext
             putContext $ updateDecodeJpegCtx ctx (CtxState SearchMarker)
             decodeJpegIter SearchMarker )
    else (do 
             ctx <- getContext
             decodeJpegIter (state ctx))

parseSOFIter :: BS.ByteString -> V.Vector FrameHeaderComponent -> V.Vector FrameHeaderComponent
parseSOFIter compStr comps =
  if (BS.null compStr)
  then comps
  else
    let c  = (fromIntegral $ BSU.unsafeIndex compStr 0) :: Word8
        h  = (fromIntegral $ BSU.unsafeIndex compStr 1) `unsafeShiftR` 4 :: Word8
        v  = (fromIntegral $ BSU.unsafeIndex compStr 1) .&. 0x0F :: Word8
        tq = (fromIntegral $ BSU.unsafeIndex compStr 2) :: Word8
    in
     parseSOFIter (BS.drop 3 compStr) (V.snoc comps (FrameHeaderComponent c h v tq))
        

parseSOF0 :: ByteStreamDecoderT DecodeJpegCtx String IO ()
parseSOF0 = do
  len <- getWord16be
  len <- checkValid len
  remain <- BSD.length
  if ((fromIntegral remain) < len - 2)
    then (do
             liftIO $ putStrLn $ "SOF0: len="++(show len)
             throwError "eof (too short)")
    else (do
             p  <- unsafeGetWord8
             y  <- unsafeGetWord16be
             x  <- unsafeGetWord16be
             nf <- unsafeGetWord8
             liftIO $ putStrLn $
               "SOF0: len="++(show len)++" "++
               "P="++(show p) ++ " " ++
               "("++(show x)++","++(show y)++")" ++ " " ++
               "Nf="++(show nf)
             compStr <- readN $ fromIntegral nf*3
             ctx <- getContext
             let newFrameHeader = FrameHeader 0 p y x (parseSOFIter compStr V.empty)
             --liftIO $ putStrLn $ show newFrameHeader
             putContext $ updateDecodeJpegCtx ctx (CtxFh newFrameHeader)
             ctx <- getContext
             let x' = fromIntegral x :: Int
             let y' = fromIntegral y :: Int
             putContext $ updateDecodeJpegCtx ctx (CtxDecodeImage (array ((0,0),(x',y')) [((i,j),(RGB 0 0 0))|i<-[0..x'],j<-[0..y']]))
             decodeJpegIter (state ctx)
         )

parseJFIF :: ByteStreamDecoderT DecodeJpegCtx String IO ()
parseJFIF = do
  ver <- getWord16be
  ver <- checkValid ver
  u   <- getWord8
  u   <- checkValid u
  xd  <- getWord16be
  xd  <- checkValid xd
  yd  <- getWord16be
  yd  <- checkValid yd
  xt  <- getWord8
  xt  <- checkValid xt
  yt  <- getWord8
  yt  <- checkValid yt
  liftIO $ putStrLn $ "JFIF: ver=0x" ++ (showHex ver "") ++
                      " u=" ++ (show u) ++
                      " xd=" ++ (show xd) ++
                      " yd=" ++ (show yd) ++
                      " xt=" ++ (show xt) ++
                      " yt=" ++ (show yt)
  parseThumnail xt yt
  where
    parseThumnail xt yt
      | (xt == 0) && (yt == 0) = do
        ctx <- getContext
        decodeJpegIter (state ctx)
      | (xt /= 0) && (yt /= 0) = throwError "need impliment decode thumnail"
      | otherwise = throwError "invalid value"
             
parseAPP0 :: ByteStreamDecoderT DecodeJpegCtx String IO ()
parseAPP0 = do
  len <- getWord16be
  len <- checkValid len
  -- len は Maker分の0xFFxxを含む
  idstr <- readN 5
  liftIO $ putStrLn $ "Maker:APP0,len="++(show len)++",id="++(show idstr)
  case idstr of
    "JFIF\0" -> (do
                    ctx <- getContext
                    putContext $ updateDecodeJpegCtx ctx (CtxFormat idstr)
                    parseJFIF)
    _ -> (do
             liftIO $ putStrLn "parse JFXX?"
             return ())

parseAPP1 :: ByteStreamDecoderT DecodeJpegCtx String IO ()
parseAPP1 = do
  len <- getWord16be
  len <- checkValid len
  remain <- BSD.length
  let exiflen = fromIntegral $ len - 2 :: Int64
  if remain < exiflen
    then throwError "eof"
    else (do
             exif  <- readN exiflen
             parseTIFF exif
             ctx <- getContext
             decodeJpegIter (state ctx))
  
parseAPP13 :: ByteStreamDecoderT DecodeJpegCtx String IO ()
parseAPP13 = do
  len <- getWord16be
  len <- checkValid len
  liftIO $ putStrLn $ "Maker:APP13,len="++(show len)
  BSD.drop $ fromIntegral (len-2)
  ctx <- getContext
  decodeJpegIter (state ctx)

parseMarker :: Word8 -> ByteStreamDecoderT DecodeJpegCtx String IO ()
parseMarker code =
  case code of
    0xC0 -> parseSOF0
    0xC4 -> parseDHT decodeJpegIter
    0xDB -> parseDQT decodeJpegIter
    0xDA -> parseSOS decodeJpegIter
    0xE0 -> parseAPP0
    0xE1 -> parseAPP1
    0xED -> parseAPP13
    _    -> do
      ctx <- getContext
      --liftIO $ putStrLn $ show ctx 
      throwError $ "unknown maker code: 0x" ++ (showHex code "")
    
searchMarker :: ByteStreamDecoderT DecodeJpegCtx String IO ()
searchMarker = do
  liftIO $ putStrLn "searchMarker"
  c0  <- getWord8
  c0' <- checkValid c0
  if (c0' == 0xFF)
    then (do
             c0 <- getWord8
             c0' <- checkValid c0
             parseMarker c0')
    else (do
             liftIO $ putStrLn "find no marker"
             searchMarker )
         
decodeJpegIter :: DecodeJpegState -> ByteStreamDecoderT DecodeJpegCtx String IO ()
decodeJpegIter state = case state of
  SearchSOI -> searchSOI
  SearchMarker -> searchMarker
  ReadEntropyCodedSegment -> decodeEntropy decodeJpegIter
  DecodeExit -> (do
                    liftIO $ putStrLn "Exit Decoder"
                    return ())
  
decodeJpeg :: ByteStreamDecoderT DecodeJpegCtx String IO ()
decodeJpeg = do
  ctx <- getContext
  decodeJpegIter (state ctx)

runJpegDecoder :: BSL.ByteString -> Handle -> IO (Either String ())
runJpegDecoder input output = do
  r <- runByteStreamDecoderT decodeJpeg (initDecodeJpegCtx output) input
  return r
