{-# LANGUAGE CPP, MagicHash #-}

module Data.Binary.ByteStream (
  ByteStream,
  ByteStreamT(..),
  drop,
  take,
  length,
  readN,
  getWord8,
  getWord16le,
  getWord16be,
  getWord32le,
  getWord32be,
  peekWord8,
  unsafeGetWord8,
  unsafeGetWord16le,
  unsafeGetWord16be,
  liftIO,
  liftCatch
  ) where

import Prelude hiding (length,drop,take)

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Fix
import Control.Monad.Signatures
import Control.Monad.Trans.Except
import Data.Functor.Identity

import Data.Bits
import Data.Word
import Data.Int

import GHC.Base
import GHC.Word

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU

newtype ByteStreamT m a = ByteStreamT { runByteStreamT :: BSL.ByteString -> m (a, BSL.ByteString) }

instance (Functor m) => Functor (ByteStreamT m) where
  fmap f m = ByteStreamT $ \ctx ->
    fmap (\ ~(a, ctx') -> (f a, ctx')) $ runByteStreamT m ctx
  {-# INLINE fmap #-}

instance (Functor m, Monad m) => Applicative (ByteStreamT m) where
  pure a = ByteStreamT $ \ctx -> return (a, ctx)
  {-# INLINE pure #-}
  ByteStreamT mf <*> ByteStreamT mx = ByteStreamT $ \ctx -> do
    ~(f, ctx') <- mf ctx
    ~(x, ctx'') <- mx ctx'
    return (f x, ctx'')
  {-# INLINE (<*>) #-}

instance (Functor m, MonadPlus m) => Alternative (ByteStreamT m) where
  empty = ByteStreamT $ \ _ -> mzero
  {-# INLINE empty #-}
  ByteStreamT m <|> ByteStreamT n = ByteStreamT $ \ctx -> m ctx `mplus` n ctx 
  
instance (Monad m) => Monad (ByteStreamT m) where
  return a = ByteStreamT $ \ctx -> return (a, ctx)
  {-# INLINE return #-}
  m >>= k = ByteStreamT $ \ctx -> do
    ~(a, ctx') <- runByteStreamT m ctx
    runByteStreamT (k a) ctx'
  {-# INLINE (>>=) #-}
  fail str = ByteStreamT $ \_ -> fail str
  {-# INLINE fail #-}

instance (MonadPlus m) => MonadPlus (ByteStreamT m) where
  mzero = ByteStreamT $ \_ -> mzero
  {-# INLINE mzero #-}
  ByteStreamT m `mplus` ByteStreamT n = ByteStreamT $ \ctx -> m ctx `mplus` n ctx
  {-# INLINE mplus #-}

instance (MonadFix m) => MonadFix (ByteStreamT m) where
  mfix f = ByteStreamT $ \ctx -> mfix $ \ ~(a, _) -> runByteStreamT (f a) ctx
  {-# INLINE mfix #-}

instance MonadTrans (ByteStreamT) where
  lift m = ByteStreamT $ \ctx -> do
    a <- m
    return (a, ctx)
  {-# INLINE lift #-}

instance (MonadIO m) => MonadIO (ByteStreamT m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

length :: (Monad m) => ByteStreamT m Int64
length = byteStream $ \ctx -> (BSL.length ctx, ctx)

readN :: (Monad m) => Int64 -> ByteStreamT m BS.ByteString
readN n = byteStream $ \ctx -> (do
                                   let bs = BSL.toStrict $ BSL.take n ctx
                                   (bs, (BSL.drop n ctx)))

take :: (Monad m) => Int64 -> ByteStreamT m BS.ByteString
take n = byteStream $ \ctx -> do
  let bs = BSL.toStrict $ BSL.take n ctx
  if BSL.null ctx
    then (BS.empty, ctx)
    else (bs, ctx)
  
drop :: (Monad m) => Int64 -> ByteStreamT m ()
drop n = byteStream $ \ctx -> do
  if BSL.null ctx
    then ((), ctx)
    else ((), (BSL.drop n ctx))

getWord8 :: (Monad m) => ByteStreamT m (Maybe Word8)
getWord8 = byteStream $ \ctx -> do
  if BSL.null ctx
    then (Nothing, ctx)
    else (Just (BSL.head ctx), (BSL.drop 1 ctx))

getWord16le :: (Monad m) => ByteStreamT m (Maybe Word16)
getWord16le = do
  l <- length
  if (l < 2)
    then return Nothing
    else (do 
             bin <- readN 2
             let c0 = fromIntegral $ BSU.unsafeIndex bin 0 :: Word16
                 c1 = fromIntegral $ BSU.unsafeIndex bin 1 :: Word16
             return $ Just $ (c1 `shiftl_w16` 8) .|. c0
         )

getWord16be :: (Monad m) => ByteStreamT m (Maybe Word16)
getWord16be = do
  l <- length
  if (l < 2)
    then return Nothing
    else (do
             bin <- readN 2
             let c0 = fromIntegral (BSU.unsafeIndex bin 0) :: Word16
                 c1 = fromIntegral (BSU.unsafeIndex bin 1) :: Word16
             return $ Just $ (c0 `shiftl_w16` 8) .|. c1
         )

getWord32le :: (Monad m) => ByteStreamT m (Maybe Word32)
getWord32le = do
  l <- length
  if (l < 4)
    then return Nothing
    else (do 
             bin <- readN 4
             let c0 = fromIntegral (BSU.unsafeIndex bin 0) :: Word32
                 c1 = fromIntegral (BSU.unsafeIndex bin 1) :: Word32
                 c2 = fromIntegral (BSU.unsafeIndex bin 2) :: Word32
                 c3 = fromIntegral (BSU.unsafeIndex bin 3) :: Word32
             return $ Just $ (c3 `shiftl_w32` 24) .|. (c2 `shiftl_w32` 16) .|. (c1 `shiftl_w32` 8) .|. c0
         )

getWord32be :: (Monad m) => ByteStreamT m (Maybe Word32)
getWord32be = do
  l <- length
  if (l < 4)
    then return Nothing
    else (do 
             bin <- readN 4
             let c0 = fromIntegral (BSU.unsafeIndex bin 0) :: Word32
                 c1 = fromIntegral (BSU.unsafeIndex bin 1) :: Word32
                 c2 = fromIntegral (BSU.unsafeIndex bin 2) :: Word32
                 c3 = fromIntegral (BSU.unsafeIndex bin 3) :: Word32
             return $ Just $ (c0 `shiftl_w32` 24) .|. (c1 `shiftl_w32` 16) .|. (c2 `shiftl_w32` 8) .|. c3
         )

peekWord8 :: (Monad m) => ByteStreamT m (Maybe Word8)
peekWord8 = byteStream $ \ctx -> do
  if BSL.null ctx
    then (Nothing, ctx)
    else (Just (BSL.head ctx), ctx)


unsafeGetWord8 :: (Monad m) => ByteStreamT m Word8
unsafeGetWord8 = byteStream $ \ctx -> 
   ((BSL.head ctx), (BSL.drop 1 ctx))


unsafeGetWord16le :: (Monad m) => ByteStreamT m Word16
unsafeGetWord16le = do
  bin <- readN 2
  let c0 = fromIntegral $ BSU.unsafeIndex bin 0 :: Word16
      c1 = fromIntegral $ BSU.unsafeIndex bin 1 :: Word16
  return $ (c1 `shiftl_w16` 8) .|. c0

unsafeGetWord16be :: (Monad m) => ByteStreamT m Word16
unsafeGetWord16be = do
  bin <- readN 2
  let c0 = fromIntegral $ BSU.unsafeIndex bin 0 :: Word16
      c1 = fromIntegral $ BSU.unsafeIndex bin 1 :: Word16
  return $ (c0 `shiftl_w16` 8) .|. c1

type ByteStream = ByteStreamT Identity

byteStream :: (Monad m) => (BSL.ByteString -> (a, BSL.ByteString)) -> ByteStreamT m a
byteStream f = ByteStreamT ( return . f )

liftCatch :: Catch e m (a,BSL.ByteString) -> Catch e (ByteStreamT m) a
liftCatch catchE m h =
  ByteStreamT $ \ctx -> runByteStreamT m ctx `catchE` \e -> runByteStreamT (h e) ctx

-- unchecked shifts
shiftl_w16 :: Word16 -> Int -> Word16
shiftl_w32 :: Word32 -> Int -> Word32
shiftl_w64 :: Word64 -> Int -> Word64

shiftl_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftL#` i)
shiftl_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftL#` i)
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL#` i)
