{-# LANGUAGE CPP, MagicHash #-}

module Data.Binary.ByteStreamDecoder (
  ByteStreamDecoderT,
  runByteStreamDecoderT,
  getContext,
  putContext,
  length,
  take,
  drop,
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
  throwError,
  lift,
  liftIO
  ) where

import Prelude hiding (length, take, drop)

import qualified Control.Monad.Except as EX
import Control.Monad.Except (ExceptT,runExceptT)
import Control.Monad.State 

import Data.Int
import Data.Word

import qualified Data.Binary.ByteStream as BST

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

--type ByteStreamDecoderT c e m a = StateT c (BST.ByteStreamT (ExceptT e m)) a
type ByteStreamDecoderT c e m = StateT c (BST.ByteStreamT (ExceptT e m)) 

runByteStreamDecoderT :: (Monad m) => ByteStreamDecoderT c e m a -> c -> BSL.ByteString -> m (Either e a)
runByteStreamDecoderT decoder ctx input =
  runExceptT (do
                 (r, _) <- BST.runByteStreamT (do
                                                  (r, _) <- runStateT decoder ctx
                                                  return r) input
                 return r)
  
getContext :: (MonadState c m) => m c
getContext = get

putContext :: (MonadState c m) => c -> m ()
putContext = put

length :: (Monad m) => ByteStreamDecoderT c e m Int64
length = lift $ BST.length

take :: (Monad m) => Int64 -> ByteStreamDecoderT c e m BS.ByteString
take n = lift $ BST.take n

drop :: (Monad m) => Int64 -> ByteStreamDecoderT c e m ()
drop n = lift $ BST.drop n

readN :: (Monad m) => Int64 -> ByteStreamDecoderT c e m BS.ByteString
readN n = lift $ BST.readN n

getWord8 :: (Monad m) => ByteStreamDecoderT c e m (Maybe Word8)
getWord8 = lift BST.getWord8

getWord16le :: (Monad m) => ByteStreamDecoderT c e m (Maybe Word16)
getWord16le = lift BST.getWord16le

getWord16be :: (Monad m) => ByteStreamDecoderT c e m (Maybe Word16)
getWord16be = lift BST.getWord16be

getWord32le :: (Monad m) => ByteStreamDecoderT c e m (Maybe Word32)
getWord32le = lift BST.getWord32le

getWord32be :: (Monad m) => ByteStreamDecoderT c e m (Maybe Word32)
getWord32be = lift BST.getWord32be

peekWord8 :: (Monad m) => ByteStreamDecoderT c e m (Maybe Word8)
peekWord8 = lift BST.peekWord8

unsafeGetWord8 :: (Monad m) => ByteStreamDecoderT c e m Word8
unsafeGetWord8 = lift BST.unsafeGetWord8

unsafeGetWord16le :: (Monad m) => ByteStreamDecoderT c e m Word16
unsafeGetWord16le = lift BST.unsafeGetWord16le

unsafeGetWord16be :: (Monad m) => ByteStreamDecoderT c e m Word16
unsafeGetWord16be = lift BST.unsafeGetWord16be

throwError :: (EX.MonadError e m, MonadTrans t0, MonadTrans t1, Monad (t1 m)) => e -> t0 (t1 m) a
throwError e = lift . lift $ EX.throwError e
