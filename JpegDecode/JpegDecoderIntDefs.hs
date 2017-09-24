{-# LANGUAGE RankNTypes #-}
module JpegDecoder.InternalDefs (
  RGB(..),
  DecodeJpegState(..),
  DecodeJpegCtx(..),
  DQT(..),
  FrameHeaderComponent(..),
  FrameHeader(..),
  HuffmanCode(..),
  ScanHeader(..),
  ComponentIDForScan(..),
  DecodeJpegCtxMember(..),
  initDecodeJpegCtx,
  updateDecodeJpegCtx,
  checkValid
  ) where

import System.IO

import Data.Maybe
import Data.Word 

import Data.Array

import qualified Data.Vector as V
import qualified Data.ByteString as BS
import Data.Binary.ByteStreamDecoder as BSD

data DecodeJpegState = SearchSOI|
                       SearchMarker|
                       ReadEntropyCodedSegment|
                       DecodeExit deriving (Show, Eq)

data FrameHeaderComponent = FrameHeaderComponent {
  fhcC  :: Word8,
  fhcH  :: Word8,
  fhcV  :: Word8,
  fhcTq :: Word8
  } deriving (Show)

data FrameHeader = FrameHeader {
  fhKind  :: Word8,
  fhP     :: Word8,  --サンプル精度,一画素中一成分あたりのビット数
  fhY     :: Word16, --縦サイズ
  fhX     :: Word16, --横サイズ
  comps :: V.Vector FrameHeaderComponent
  } deriving (Show)

data DQT = DQT {
  dqtPq :: Word8,
  dqtTq :: Word8,
  dqtQs :: V.Vector Word8
  } deriving (Show)

data ComponentIDForScan = ComponentIDForScan {
  csi   :: Word8,
  citd  :: Word8,
  cita  :: Word8
  } deriving (Show)

data ScanHeader = ScanHeader {
  compIDs         :: V.Vector ComponentIDForScan,
  startOfSpectral :: Word8,
  endOfSpectral   :: Word8,
  approxHigh      :: Word8,
  approxLow       :: Word8
  } deriving (Show)

data DecodeJpegCtx = DecodeJpegCtx {
  hOutput      :: Handle,
  state        :: DecodeJpegState,
  format       :: BS.ByteString,
  dqts         :: V.Vector DQT,
  frameHeader  :: FrameHeader,
  dhtArray     :: Array (Int, Int) (Array Int (V.Vector HuffmanCode)),  -- (th,tc) (Array lenghtOfBits (V.Vector HuffmanCode))
  scanHeader   :: ScanHeader,
  decodeImage  :: Array (Int, Int) RGB
  } deriving (Show)

--length code valueの順に並ぶ
data HuffmanCode = HuffmanCode Int Word16 Word8 deriving (Show)
data RGB = RGB Word8 Word8 Word8 deriving (Show)

data DecodeJpegCtxMember = CtxhOutput Handle |
                           CtxState DecodeJpegState|
                           CtxFormat BS.ByteString|
                           CtxDqts (V.Vector DQT)|
                           CtxDhts (Array (Int, Int) (Array Int (V.Vector HuffmanCode)))|
                           CtxScanHeader ScanHeader |
                           CtxDecodeImage (Array (Int, Int) RGB) |
                           CtxFh FrameHeader

initDecodeJpegCtx output =
  let
    emptyCodes      = array (1,16) [(i,V.empty)|i<-[1..16]]
    initDht         = array ((0,0),(1,1)) [ ((i,j),emptyCodes) | i <- [0..1], j <-[0..1]]  :: Array (Int, Int) (Array Int (V.Vector HuffmanCode))
    initScanHeader  =  ScanHeader V.empty 0 0 0 0
    initDecodeImage = array ((0,0),(0,0)) [((0,0),(RGB 0 0 0))]
    initFrameHeader = FrameHeader 0 0 0 0 V.empty
  in
   DecodeJpegCtx output SearchSOI BS.empty V.empty initFrameHeader initDht initScanHeader initDecodeImage

updateDecodeJpegCtx :: DecodeJpegCtx -> DecodeJpegCtxMember -> DecodeJpegCtx

updateDecodeJpegCtx ctx (CtxhOutput updateMember) =
  DecodeJpegCtx updateMember (state ctx) (format ctx) (dqts ctx) (frameHeader ctx) (dhtArray ctx) (scanHeader ctx) (decodeImage ctx)

updateDecodeJpegCtx ctx (CtxState updateMember) =
  DecodeJpegCtx (hOutput ctx) updateMember (format ctx) (dqts ctx) (frameHeader ctx) (dhtArray ctx) (scanHeader ctx) (decodeImage ctx)

updateDecodeJpegCtx ctx (CtxFormat updateMember) =
  DecodeJpegCtx (hOutput ctx) (state ctx) updateMember (dqts ctx) (frameHeader ctx) (dhtArray ctx) (scanHeader ctx) (decodeImage ctx)

updateDecodeJpegCtx ctx (CtxDqts updateMember) =
  DecodeJpegCtx (hOutput ctx) (state ctx) (format ctx) updateMember (frameHeader ctx) (dhtArray ctx) (scanHeader ctx) (decodeImage ctx)

updateDecodeJpegCtx ctx (CtxFh updateMember) =
  DecodeJpegCtx (hOutput ctx) (state ctx) (format ctx) (dqts ctx) updateMember (dhtArray ctx) (scanHeader ctx) (decodeImage ctx)

updateDecodeJpegCtx ctx (CtxDhts updateMember) =
  DecodeJpegCtx (hOutput ctx) (state ctx) (format ctx) (dqts ctx) (frameHeader ctx) updateMember (scanHeader ctx) (decodeImage ctx)

updateDecodeJpegCtx ctx (CtxScanHeader updateMember) =
  DecodeJpegCtx (hOutput ctx) (state ctx) (format ctx) (dqts ctx) (frameHeader ctx) (dhtArray ctx) updateMember (decodeImage ctx)

updateDecodeJpegCtx ctx (CtxDecodeImage updateMember) =
  DecodeJpegCtx (hOutput ctx) (state ctx) (format ctx) (dqts ctx) (frameHeader ctx) (dhtArray ctx) (scanHeader ctx) updateMember
  
checkValid :: (Monad m) => Maybe a -> ByteStreamDecoderT c String m a
checkValid a = do
  if isJust a
    then (do
             let (Just a') = a
             return a')
    else throwError "eof"
