{-# LANGUAGE CPP, OverloadedStrings #-}

module JpegDecoder.DecodeEntropy (
  decodeEntropy
  ) where

import Data.Maybe
import Data.Bits
import Data.Word
import Data.Array

import Control.Monad.State

import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

import Data.Binary.ByteStreamDecoder as BSD

import JpegDecoder.InternalDefs

import Debug.Trace 

data DecodeMCUContext = DecodeMCUContext {
  remainBits :: (Int, Word8),
  lums :: V.Vector (Array (Int, Int) Word8),
  cbs  :: V.Vector (Array (Int, Int) Word8),
  crs  :: V.Vector (Array (Int, Int) Word8),
  dctMatrix :: Array (Int,Int) Int
  }

showHex :: Int -> String
showHex n = iter n ""
  where
    hextable = array (0,15) [(0,"0"),(1,"1"),(2,"2"),(3,"3"),(4,"4"),(5,"5"),(6,"6"),(7,"7"),
                             (8,"8"),(9,"9"),(10,"A"),(11,"B"),(12,"C"),(13,"D"),(14,"E"),(15,"F")]
    iter n str =
      if (n == 0)
      then "0x" ++ if (null str)
                   then "0"
                   else str
      else iter (div n 16) (hextable!(mod n 16) ++ str)

getBit :: StateT DecodeMCUContext (ByteStreamDecoderT DecodeJpegCtx String IO) Word16
getBit = do
  ctx <- get
  let (remainL, remainB) = remainBits ctx
  if (remainL == 0)
    then (do
             --liftIO $ putStrLn $ "read byte"
             b <- lift getWord8
             b <- lift $ checkValid b
             --liftIO $ putStrLn $ showHex $ fromIntegral b
             put $ DecodeMCUContext (7,b) (lums ctx) (cbs ctx) (crs ctx) (dctMatrix ctx)
             return $ fromIntegral $ (b `unsafeShiftR` 7) .&. 0x01)
    else (do
             --liftIO $ putStrLn $ "remain bits="++(show remainL)++","++(showHex $ fromIntegral remainB)
             put $ DecodeMCUContext (remainL-1,remainB) (lums ctx) (cbs ctx) (crs ctx) (dctMatrix ctx)
             return $ fromIntegral $ (remainB `unsafeShiftR` (remainL-1)) .&. 0x01)

getBits :: Int -> StateT DecodeMCUContext (ByteStreamDecoderT DecodeJpegCtx String IO) Word16
getBits n = iter 0 0
  where
    iter i bits =
      if (i < n)
      then (do
               b <- getBit
               --liftIO $ putStr $ (show b)
               iter (i+1) ((bits `unsafeShiftL` 1) .|. b))
      else return bits
  

getNumOfComps :: DecodeJpegCtx -> Int
getNumOfComps ctx = V.length $ comps $ frameHeader ctx  

getMaxOfComp :: (FrameHeaderComponent -> Word8) -> V.Vector FrameHeaderComponent -> Int
getMaxOfComp selector fhcs = iter fhcs 0
  where
    iter fhcs max = 
      if (V.null fhcs)
      then fromIntegral max
      else
        let
          h = selector $ V.head fhcs
          newMax = if (max < h) then h else max
        in 
         iter (V.drop 1 fhcs) newMax
         
getMaxOfH :: V.Vector FrameHeaderComponent -> Int
getMaxOfH fhcs = getMaxOfComp fhcH fhcs

getMaxOfV :: V.Vector FrameHeaderComponent -> Int
getMaxOfV fhcs = getMaxOfComp fhcV fhcs

getBlocks :: Int -> Int
--getBlocks n = (n `unsafeShiftR` 3) + if ( (n .&. 0x07) > 0 ) then 1 else 0 
getBlocks n = (n `div` 8) + if ( (n `mod` 8) > 0 ) then 1 else 0 

searchCodeAndReturnValue :: V.Vector HuffmanCode -> Word16 -> Maybe Word8
searchCodeAndReturnValue huffmanCodeList code =
  if (V.null huffmanCodeList)
  then Nothing
  else
    let
      matchCode = V.find (\(HuffmanCode len pattern value)->(pattern == code)) huffmanCodeList
    in
     if (isJust matchCode)
     then
       let 
         selectValue (HuffmanCode len pattern value) = value
       in
        Just (selectValue (fromJust matchCode))
     else
       Nothing

decodeHuffmanDC :: Array Int (V.Vector HuffmanCode) -> StateT DecodeMCUContext (ByteStreamDecoderT DecodeJpegCtx String IO) Int
decodeHuffmanDC huffmanTbl = do
  --liftIO $ putStrLn $ "decodeHuffmanDC"
  category <- iterHuffman 1 0
  --liftIO $ putStrLn $ "category=" ++ (show category) 
  diff <- getBits $ fromIntegral category
  --liftIO $ putStrLn $ "read bits " ++ (showHex $ fromIntegral diff)
  let isNegative = (diff .&. ( 1 `unsafeShiftL` (fromIntegral (category-1)))) == 0
  if isNegative
    then return $ (fromIntegral diff) - ((1 `unsafeShiftL` (fromIntegral category)) - 1)
    else return $ (fromIntegral diff)  
  where
    iterHuffman :: Int -> Word16 ->  StateT DecodeMCUContext (ByteStreamDecoderT DecodeJpegCtx String IO) Word8
    iterHuffman length code = do
      --liftIO $ putStrLn $ "iterHuffmanDC" ++ "(" ++ (show length) ++ "," ++ (showHex $ fromIntegral code) ++ ")"
      if (length <= 16)
        then (do
                 b <- getBit
                 let code' = code .|. b
                 let transCode = searchCodeAndReturnValue (huffmanTbl ! length) code'
                 if (isJust transCode)
                   then return $ fromJust transCode
                   else iterHuffman (length+1) (code' `unsafeShiftL` 1))
        else return 0

-- (runLength,ac value)
decodeHuffmanAC :: Array Int (V.Vector HuffmanCode) -> StateT DecodeMCUContext (ByteStreamDecoderT DecodeJpegCtx String IO) (Int, Int)
decodeHuffmanAC huffmanTbl = do
  --liftIO $ putStrLn $ "decodeHuffmanAC"
  v <- iterHuffman 1 0
  return v
  where
    iterHuffman :: Int -> Word16 -> StateT DecodeMCUContext (ByteStreamDecoderT DecodeJpegCtx String IO) (Int,Int)
    iterHuffman length code = do
      --liftIO $ putStrLn $ "iterHuffman" ++ "(" ++ (show length) ++ "," ++ (showHex $ fromIntegral code) ++ ")"
      if (length <= 16)
        then (do
                 b <- getBit
                 let code' = code .|. b
                 let transCode = searchCodeAndReturnValue (huffmanTbl ! length) code'
                 if (isJust transCode)
                   then
                   let 
                     (runLength, category) = (\c -> (c `unsafeShiftR` 4, c .&. 0x0F)) (fromIntegral $ fromJust transCode)                     
                     isNegative v = (v .&. (1 `unsafeShiftL` (category-1))) == 0 
                   in
                    case (runLength, category) of
                      (0, 0) -> return (0,0) -- EOB (End of Bits)
                      (_, _) 
                        | (runLength > 15) -> lift $ throwError "Invalid runlength in AC"
                        | otherwise -> (do
                                           acv <- getBits category
                                           let acv' =
                                                 if (isNegative $ acv)
                                                 then (fromIntegral acv) - ((1 `unsafeShiftL` (fromIntegral category)) - 1)
                                                 else fromIntegral acv                                           
                                           return (runLength, acv'))
                   else iterHuffman (length+1) (code' `unsafeShiftL` 1))
        else lift $ throwError "Invalid decode Length"

decodeHuffman :: Int -> ComponentIDForScan -> StateT DecodeMCUContext (ByteStreamDecoderT DecodeJpegCtx String IO) Bool
decodeHuffman  n (ComponentIDForScan cs td ta) = do
  decodeJpegCtx <- lift getContext
  --liftIO $ putStrLn $ show (dhtArray decodeJpegCtx)
  --liftIO $ putStrLn $ "n=" ++ (show n) ++ ",Cs=" ++ (show cs) ++ ",Td="++(show td)++ ",Ta="++(show ta)
  let huffmanTbl = (dhtArray decodeJpegCtx)!( fromIntegral $ if (n == 0) then td else ta, if (n == 0) then 0 else 1);
  if (n == 0)
    then (do
             v <- decodeHuffmanDC huffmanTbl
             liftIO $ putStrLn $ show v
             return True)
    else (do
             (runLength,acv) <- decodeHuffmanAC huffmanTbl          
             liftIO $ putStrLn $ show (runLength, acv)
             case (runLength,acv) of
               (0,0) -> return False
               (_,_) -> return True)

parseMCU :: V.Vector Int -> StateT DecodeMCUContext (ByteStreamDecoderT DecodeJpegCtx String IO) ()
parseMCU sizesOfComp = sizesOfCompIter 0 sizesOfComp
  where
    -- 8x8(64)画素をデコード
    compIter :: Int -> Int -> Array (Int, Int) Word8 -> StateT DecodeMCUContext (ByteStreamDecoderT DecodeJpegCtx String IO) ()
    compIter indexOfComp n matrixOfData =
      if (n < 64)
      then (do
               decodeJpegCtx <- lift getContext 
               let compIDForScan = (compIDs $ scanHeader decodeJpegCtx) `V.unsafeIndex` indexOfComp
               cont <- decodeHuffman n compIDForScan
               if cont
                 then compIter indexOfComp (n+1) matrixOfData
                 else (do
                          liftIO $ putStrLn $ "find EOB"
                          return ()))
      else return ()
    -- YUVのBlockをデコード
    blockIter indexOfComp numOfBlock =
      if (numOfBlock == 0)
      then return ()
      else
        (do
            compIter indexOfComp 0 (array ((0,0),(7,7)) [((i,j),0)|i<-[0..7],j<-[0..7]])
            blockIter indexOfComp (numOfBlock-1))
    -- indexOfComp->画素成分のインデックス, sizesOfComp->画素成分の大きさ
    sizesOfCompIter indexOfComp sizesOfComp =
      if (V.null sizesOfComp)
      then return ()
      else (do
               liftIO $ putStrLn $ (show indexOfComp) ++ ":" ++ (show $ V.head sizesOfComp)               
               blockIter indexOfComp (V.head sizesOfComp)
               sizesOfCompIter(indexOfComp + 1) (V.drop 1 sizesOfComp))

decodeMCU :: StateT DecodeMCUContext (ByteStreamDecoderT DecodeJpegCtx String IO) ()
decodeMCU = do
  decodeJpegCtx <- lift getContext
  let numOfComps = getNumOfComps decodeJpegCtx
  let maxOfV     = getMaxOfV $ comps $ frameHeader decodeJpegCtx
  let maxOfH     = getMaxOfH $ comps $ frameHeader decodeJpegCtx
  let vBlocks    = getBlocks $ fromIntegral $ fhY $ frameHeader decodeJpegCtx
  let hBlocks    = getBlocks $ fromIntegral $ fhX $ frameHeader decodeJpegCtx
  let numOfVUnit = (vBlocks `div` maxOfV) + if ((vBlocks `mod` maxOfV) > 0) then 1 else 0
  let numOfHUnit = (hBlocks `div` maxOfH) + if ((hBlocks `mod` maxOfH) > 0) then 1 else 0
  liftIO $ putStrLn $ "vBlocks=" ++ (show vBlocks) ++ ",hBlocks=" ++ (show hBlocks)
  liftIO $ putStrLn $ "maxOfV=" ++ (show maxOfV) ++ ",maxOfH=" ++ (show maxOfH)
  liftIO $ putStrLn $ "numOfVUnit=" ++ (show numOfVUnit) ++ ",numOfHUnit=" ++ (show numOfHUnit)
  sizesOfComp <- compIter V.empty 0 numOfComps
  parseMCU sizesOfComp
  return ()
  where
    compIter :: (V.Vector Int) -> Int -> Int -> StateT DecodeMCUContext (ByteStreamDecoderT DecodeJpegCtx String IO) (V.Vector Int)
    compIter v i numOfComps = do
      decodeJpegCtx <- lift getContext
      let fhc = V.unsafeIndex (comps $ frameHeader decodeJpegCtx) i
      if (i < numOfComps)
        then (do
                 liftIO $ putStrLn $ "C="++(show $ fhcC fhc)++",H="++(show $ fhcH fhc) ++  ",V="++(show $ fhcV fhc)
                 compIter (V.snoc v (fromIntegral ((fhcH fhc)*(fhcV fhc)))) (i+1) numOfComps)
        else return v
                 
  
decodeEntropy :: (DecodeJpegState -> ByteStreamDecoderT DecodeJpegCtx String IO ()) -> ByteStreamDecoderT DecodeJpegCtx String IO ()
decodeEntropy decodeJpegIter =
  let
    initDecodeMCUContext = DecodeMCUContext (0,0)
                                            V.empty
                                            V.empty
                                            V.empty
                                            (array ((0,0),(7,7)) [((i,j), 0)|i<-[0..7],j<-[0..7]])
  in 
   evalStateT decodeMCU initDecodeMCUContext
