{-# LANGUAGE CPP, OverloadedStrings #-}

module JpegDecoder.Dht (
  parseDHT,
  showHuffmanCodeList
  ) where

import Prelude as P

import Data.Bits
import Data.Word
import Data.Array

import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

import Data.Binary.ByteStreamDecoder as BSD

import JpegDecoder.InternalDefs

showL :: [Word8] -> Int -> Int -> String -> String
showL ls i maxOfi nowStr =
  if i <= maxOfi
  then showL (P.drop 1 ls) (i+1) maxOfi (nowStr ++ "L" ++ (show i) ++ "=" ++ (show $ P.head ls) ++ ",") 
  else nowStr

showBits :: Int -> Word16 -> String
showBits lenOfBits bits = iter lenOfBits bits ""
  where
    iter :: Int -> Word16 ->  String -> String
    iter i bits str =
      if i == 0
      then "("++(show bits)++")" ++ str
      else
        let
          b = if ((bits .&. (1 `unsafeShiftL` (i-1))) == 0)
              then "0"
              else "1"
        in
         iter (i-1) bits (str ++ b)

showHuffmanCode :: V.Vector HuffmanCode -> String
showHuffmanCode codes = iter codes ""
  where
    iter codes str =
      if (V.null codes)
      then str
      else iter (V.drop 1 codes) (str ++ (showCode (V.head codes)) ++ "\n")
    showCode (HuffmanCode len bits value) =
      "length:" ++ (show len) ++ ",bits:" ++ (showBits len bits) ++ ",value:" ++ (show value)
      
showHuffmanCodeList :: Array Int (V.Vector HuffmanCode) -> String
showHuffmanCodeList list = iter 1 list ""
  where
    iter i list str =
      if i==17
      then str
      else
        let
          newString = showHuffmanCode $ list!i
        in 
         iter (i+1) list (str ++ "length of bits:" ++ (show i) ++ "\n" ++ newString)

parseDHTSymbolIter :: Int -> Array Int (V.Vector Word32) -> [Word8] -> BS.ByteString -> Array Int (V.Vector Word32)
parseDHTSymbolIter i table ls dht =
  if (BS.null dht)
  then table
  else
    let
      readCode len codes dht =
        if len == 0
        then codes
        else
          let
            c = fromIntegral (BS.head dht) :: Word32
          in
           readCode (len-1) (V.snoc codes c) (BS.drop 1 dht)
      codes  = readCode (P.head ls) V.empty dht
      table' = table // [(i,codes)]
    in
     parseDHTSymbolIter (i+1) table' (P.drop 1 ls) (BS.drop (fromIntegral $ head ls) dht)

symbolTableToHaffmanCode :: Array Int (V.Vector Word32) -> Array Int (V.Vector HuffmanCode)
symbolTableToHaffmanCode symTableList =
  iterSymbolTableList 1 symTableList 0 (array (1,16) [(i, V.empty) | i <- [1..16]])
  where
    iterSymbolTable :: Int -> (V.Vector Word32) -> Word16 -> V.Vector HuffmanCode -> (Word16, V.Vector HuffmanCode)
    iterSymbolTable lenOfBits symbolTable code v =
      if (V.null symbolTable)
      then (code, v)           
      else
        let
          code' = fromIntegral code :: Word16
          value = fromIntegral (V.head symbolTable) :: Word8
          hcode = HuffmanCode lenOfBits code' value
        in 
         iterSymbolTable lenOfBits (V.drop 1 symbolTable) (code + 1) (V.snoc v hcode)
    iterSymbolTableList :: Int -> Array Int (V.Vector Word32) -> Word16 -> Array Int (V.Vector HuffmanCode) -> Array Int (V.Vector HuffmanCode)
    iterSymbolTableList i symTableList code a =
      if i == 17
      then a
      else
        let
          symbolTable = symTableList!i
        in 
         if (V.null symbolTable)
         then iterSymbolTableList (i+1) symTableList (code*2) a
         else
           let
             (code', newV) = iterSymbolTable i symbolTable code (V.empty)
           in
            iterSymbolTableList (i+1) symTableList (code'*2) (a//[(i,newV)])
      

parseDHTIter :: DecodeJpegCtx -> BS.ByteString -> DecodeJpegCtx
parseDHTIter ctx dht = 
  let tc = fromIntegral $ (BSU.unsafeIndex dht 0) `unsafeShiftR` 4 :: Int
      th = fromIntegral $ (BSU.unsafeIndex dht 0) .&. 0x0F :: Int
      ls = [(BSU.unsafeIndex dht i) | i <- [1..16]]
      sumOfls = foldr1 (+) ls
      emptyCodes = (array (1,16) [(i,V.empty)|i<-[1..16]])
      symbolTable = symbolTableToHaffmanCode $ parseDHTSymbolIter 1 emptyCodes ls (BS.drop 17 dht)
  in        
   updateDecodeJpegCtx ctx (CtxDhts ((dhtArray ctx)//[((th,tc),symbolTable)]))
    
parseDHT :: (DecodeJpegState -> ByteStreamDecoderT DecodeJpegCtx String IO ()) -> ByteStreamDecoderT DecodeJpegCtx String IO ()
parseDHT decodeJpegIter = do
  len <- getWord16be
  len <- checkValid len
  remain <- BSD.length
  if ((fromIntegral remain) < len - 2)
    then (do
             liftIO $ putStrLn $ "DHT: len="++(show len)             
             throwError "eof (too short)")
    else (do
             liftIO $ putStrLn $ "DHT: len="++(show len)++" "
             dht <- readN $ fromIntegral (len-2)
             ctx <- getContext
             putContext $ parseDHTIter ctx dht
             ctx <- getContext
             decodeJpegIter (state ctx))
