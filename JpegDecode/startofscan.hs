{-# LANGUAGE CPP, OverloadedStrings #-}

module JpegDecoder.StartOfScan (
  parseSOS
  ) where

import Data.Bits
import Data.Word
import Data.Array

import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

import Data.Binary.ByteStreamDecoder as BSD

import JpegDecoder.InternalDefs

showScanHeader :: ScanHeader -> String
showScanHeader header =
  (iter 1 (compIDs header) "") ++
  "ss=" ++ (show $ startOfSpectral header) ++
  ",se=" ++ (show $ endOfSpectral header) ++
  ",ah=" ++ (show $ approxHigh header) ++
  ",al=" ++ (show $ approxLow header)
  where
    iter i cs str =
      if (V.null cs)
      then str
      else
        let
          c = V.head cs
        in 
         iter (i+1)
              (V.drop 1 cs)
              (str ++ "Cs" ++ (show i) ++ ":" ++ "Csi=" ++ (show $ csi c) ++ ",Td=" ++ (show $ citd c) ++ ",Ta=" ++ (show $ cita c) ++ "\n")

parseSOS :: (DecodeJpegState -> ByteStreamDecoderT DecodeJpegCtx String IO ()) -> ByteStreamDecoderT DecodeJpegCtx String IO ()
parseSOS decodeJpegIter = do
  ctx <- getContext
#if 0
  liftIO $ putStrLn $ showHuffmanCodeList ((dhtArray ctx)!(0,0))
  liftIO $ putStrLn $ showHuffmanCodeList ((dhtArray ctx)!(0,1))
  liftIO $ putStrLn $ showHuffmanCodeList ((dhtArray ctx)!(1,0))
  liftIO $ putStrLn $ showHuffmanCodeList ((dhtArray ctx)!(0,1))
#endif
  len <- getWord16be
  len <- checkValid len
  liftIO $ putStrLn $ "Maker:SOS,len="++(show len)
  ns <- getWord8
  ns <- checkValid ns
  liftIO $ putStrLn $ "Ns="++(show ns)
  stream <- readN $ fromIntegral (ns*2)
  ss <- getWord8
  ss <- checkValid ss
  se <- getWord8
  se <- checkValid se
  a  <- getWord8
  a <- checkValid a
  putContext $ updateDecodeJpegCtx ctx (CtxScanHeader $ ScanHeader (readCsIter ns stream V.empty) ss se (a `unsafeShiftR` 4) (a .&. 0x0F))
  ctx <- getContext
  liftIO $ putStrLn $ showScanHeader (scanHeader ctx) 
  putContext $ updateDecodeJpegCtx ctx (CtxState ReadEntropyCodedSegment)
  ctx <- getContext
  decodeJpegIter (state ctx)
  where
    readCsIter :: Word8 -> BS.ByteString -> V.Vector ComponentIDForScan -> V.Vector ComponentIDForScan
    readCsIter numOfComps stream comps =
      if (numOfComps == 0)
      then comps
      else
        let
          ts = BSU.unsafeIndex stream 1
          compID = ComponentIDForScan (BSU.unsafeIndex stream 0) (ts `unsafeShiftR` 4) (ts .&. 0x0F)
        in
         readCsIter (numOfComps-1) (BS.drop 2 stream) (V.snoc comps compID)
