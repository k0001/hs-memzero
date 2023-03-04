-- | This module exports tools for zeroing memory. That is,
-- filling a chunk of memory with zeros.
--
-- The exported functions behave like the ones named the same way in
-- @base@, with the only differences being that zeroing is performed
-- on the allocated memory before release, and that they are generalized
-- to run on 'MonadIO' and 'Ex.MonadMask' for your convenience.
--
-- It is recommended to @import@ this module @qualified@.
--
-- @
-- import qualified "Memzero"
-- @
--
--
module Memzero
 ( -- * memzero
   memzero
 , memzero'
   -- * alloca
 , alloca
 , allocaBytes
 , allocaBytesAligned
   -- * mallocForeignPtr
 , mallocForeignPtr
 , mallocForeignPtrBytes
 , mallocForeignPtrAlignedBytes
   -- * mallocForeignPtrConc
 , mallocForeignPtrConc
 , mallocForeignPtrConcBytes
 , mallocForeignPtrConcAlignedBytes
   -- * C finalizers
 , finalizerEnvFree
 , finalizerEnv
   -- * C support
   --
   -- $c-support
 ) --}
 where

import Control.Exception.Safe as Ex
import Control.Monad (when)
import Control.Monad.IO.Class
import Foreign.C.Types (CSize(..))
import Foreign.Marshal.Alloc qualified as A
import Foreign.Marshal.Utils qualified as A (new)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import GHC.ForeignPtr qualified as FP

--------------------------------------------------------------------------------
-- $c-support
--
-- This library also offers some tools that can be used from the C language.
-- To have access to them, you have to @#include@ the @\<hs_memzero.h\>@ header
-- that is installed together with this Haskell @memzero@ library. If you are
-- using Cabal, then this header file will be readily available for you to
-- @\#include@ without having to do anything special.
--
-- @
-- #include \<__hs_memzero.h__\>
-- @
--
-- The following C functions are exported:
--
-- @
-- /\/* This is the C version of 'memzero'' *\//
-- void __hs_memzero__(void * __p__, size_t __size__)
--
-- /\/* This is the C version of 'finalizerEnvFree' *\//
-- void __hs_memzero_finalizerEnvFree__(size_t * __size__, void * __p__)
--
-- /\/* This is the C version of 'finalizerEnv' *\//
-- void __hs_memzero_finalizerEnv__(size_t * __size__, void * __p__)
-- @
--------------------------------------------------------------------------------

-- | 'alloca' behaves exactly like @base@'s 'A.alloca', but the memory is
-- zeroed as soon as the passed in function returns.
alloca
  :: forall a b m
  .  (Storable a, MonadIO m, MonadMask m )
  => (Ptr a -> m b)
  -> m b
alloca = allocaBytesAligned
  (sizeOf (undefined :: a))
  (alignment (undefined :: a))

-- | @'allocaBytes' size@ behaves exactly like @base@'s 'A.allocaBytes', but
-- the memory is zeroed as soon as the passed in function returns.
allocaBytes
  :: forall a b m
  .  (MonadIO m, MonadMask m)
  => Int
  -> (Ptr a -> m b)
  -> m b
allocaBytes size f = do
  fp <- liftIO $ FP.mallocForeignPtrBytes size
  let p = FP.unsafeForeignPtrToPtr fp
  b <- f p `Ex.finally` memzero p size
  liftIO $ FP.touchForeignPtr fp
  pure b

-- | @'allocaBytesAligned' size alignment@ behaves exactly like @base@'s
-- 'A.allocaBytesAligned', but the memory is zeroed as soon as the passed in
-- function returns.
allocaBytesAligned
  :: forall a b m
  .  ( MonadIO m
     , MonadMask m )
  => Int
  -> Int
  -> (Ptr a -> m b)
  -> m b
allocaBytesAligned size align f = do
  fp <- liftIO $ FP.mallocForeignPtrAlignedBytes size align
  let p = FP.unsafeForeignPtrToPtr fp
  b <- f p `Ex.finally` memzero p size
  liftIO $ FP.touchForeignPtr fp
  pure b

-- | 'mallocForeignPtr' behaves exactly like @base@'s
-- 'FP.mallocForeignPtr', but the memory is zeroed
-- by a C finalizer before release.
--
-- C finalizers and 'IO' finalizers can't be mixed, so using
-- 'FP.addForeignPtrConcFinalizer' on the obtained 'FP.ForeignPtr' will fail.
-- You can only add C finalizers to it using 'FP.addForeignPtrFinalizer'.
-- If you need to add 'IO' finalizers, use 'mallocForeignPtrConc' instead.
mallocForeignPtr
  :: forall a m.
  ( Storable a
  , MonadIO m )
  => m (FP.ForeignPtr a)
mallocForeignPtr = mallocForeignPtrAlignedBytes
  (sizeOf (undefined :: a))
  (alignment (undefined :: a))

-- | @'mallocForeignPtrBytes' size@ behaves exactly like @base@'s
-- 'FP.mallocForeignPtrBytes', but the memory is zeroed
-- by a C finalizer before release.
--
-- C finalizers and 'IO' finalizers can't be mixed, so using
-- 'FP.addForeignPtrConcFinalizer' on the obtained 'FP.ForeignPtr' will fail.
-- You can only add C finalizers to it using 'FP.addForeignPtrFinalizer'.
-- If you need to add 'IO' finalizers, use 'mallocForeignPtrConcBytes'
-- instead.
mallocForeignPtrBytes
  :: forall a m
  .  (MonadIO m)
  => Int
  -> m (FP.ForeignPtr a)
mallocForeignPtrBytes size = liftIO $ do
  fp <- FP.mallocForeignPtrBytes size
  Ex.bracketOnError (A.new (fromIntegral size)) A.free $ \psize ->
    FP.addForeignPtrFinalizerEnv finalizerEnvFree psize fp
  pure fp

-- | @'mallocForeignPtrAlignedBytes' size alignment@ behaves exactly
-- like @base@'s 'FP.mallocForeignPtrAlignedBytes', but the memory is
-- zeroed by a C finalizer before release.
--
-- C finalizers and 'IO' finalizers can't be mixed, so using
-- 'FP.addForeignPtrConcFinalizer' on the obtained 'FP.ForeignPtr' will fail.
-- You can only add C finalizers to it using 'FP.addForeignPtrFinalizer'.
-- If you need to add 'IO' finalizers, use 'mallocForeignPtrConcAlignedBytes'
-- instead.
mallocForeignPtrAlignedBytes
  :: forall a m
  .  (MonadIO m)
  => Int
  -> Int
  -> m (FP.ForeignPtr a)
mallocForeignPtrAlignedBytes size align = liftIO $ do
  fp <- FP.mallocForeignPtrAlignedBytes size align
  Ex.bracketOnError (A.new (fromIntegral size)) A.free $ \psize ->
    FP.addForeignPtrFinalizerEnv finalizerEnvFree psize fp
  pure fp

-- | 'mallocForeignPtrConc' behaves exactly like @base@'s
-- 'FP.mallocForeignPtr', but the memory is zeroed
-- by an 'IO' finalizer before release.
--
-- C finalizers and 'IO' finalizers can't be mixed, so using
-- 'FP.addForeignPtrFinalizer' on the obtained 'FP.ForeignPtr' will fail.
-- You can only add 'IO' finalizers to it using 'FP.addForeignPtrConcFinalizer'.
-- If you need to add C finalizers, use 'mallocForeignPtr' instead.
mallocForeignPtrConc
  :: forall a m
  .  (Storable a, MonadIO m)
  => m (FP.ForeignPtr a)
mallocForeignPtrConc = mallocForeignPtrConcAlignedBytes
  (sizeOf (undefined :: a))
  (alignment (undefined :: a))

-- | @'mallocForeignPtrConcBytes' size@ behaves exactly like @base@'s
-- 'FP.mallocForeignPtrBytes', but the memory is zeroed
-- by an 'IO' finalizer before release.
--
-- C finalizers and 'IO' finalizers can't be mixed, so using
-- 'FP.addForeignPtrFinalizer' on the obtained 'FP.ForeignPtr' will fail.
-- You can only add 'IO' finalizers to it using 'FP.addForeignPtrConcFinalizer'.
-- If you need to add C finalizers, use 'mallocForeignPtrBytes' instead.
mallocForeignPtrConcBytes
  :: forall a m
  .  (MonadIO m)
  => Int
  -> m (FP.ForeignPtr a)
mallocForeignPtrConcBytes size = liftIO $ do
  fp <- FP.mallocForeignPtrBytes size
  liftIO $ do
    let !p = FP.unsafeForeignPtrToPtr fp
    FP.addForeignPtrConcFinalizer fp (memzero p size)
  pure fp

-- | @'mallocForeignPtrConcAlignedBytes' size alignment@ behaves exactly
-- like @base@'s 'FP.mallocForeignPtrAlignedBytes', but the memory is
-- zeroed by an 'IO' finalizer before release.
--
-- C finalizers and 'IO' finalizers can't be mixed, so using
-- 'FP.addForeignPtrFinalizer' on the obtained 'FP.ForeignPtr' will fail.
-- You can only add 'IO' finalizers to it using 'FP.addForeignPtrConcFinalizer'.
-- If you need to add C finalizers, use 'mallocForeignPtrAlignedBytes' instead.
mallocForeignPtrConcAlignedBytes
  :: forall a m
  .  (MonadIO m)
  => Int
  -> Int
  -> m (FP.ForeignPtr a)
mallocForeignPtrConcAlignedBytes size align = liftIO $ do
  fp <- FP.mallocForeignPtrAlignedBytes size align
  liftIO $ do
    let !p = FP.unsafeForeignPtrToPtr fp
    FP.addForeignPtrConcFinalizer fp (memzero p size)
  pure fp

-- | @'memzero' p size@ sets @size@ bytes starting at @p@ to zero.
--
-- This behaves like 'memzero'', but takes an 'Int' for your convenience,
-- seeing most Haskell libraries, including @base@, use 'Int' for counting
-- purposes (sic). It 'fail's if said 'Int' is negative.
memzero
  :: forall a m
  .  (MonadIO m)
  => Ptr a
  -> Int
  -> m ()
memzero p l = liftIO $ case compare l 0 of
                 GT -> _memzero p (fromIntegral l)
                 EQ -> pure ()
                 LT -> fail "memzero': negative size"

-- | @'memzero'' p size'@ sets @size@ bytes starting at @p@ to zero.
memzero'
  :: forall a m
  .  (MonadIO m)
  => Ptr a
  -> CSize
  -> m ()
memzero' p l = liftIO $ when (l > 0) (_memzero p l)

foreign import ccall unsafe "hs_memzero.h hs_memzero"
  _memzero :: Ptr a -> CSize -> IO ()

-- | This 'FP.FinalizerEnvPtr' zeroes 'CSize' bytes starting at @'Ptr' a@,
-- and afterwards 'A.free's the @'Ptr' 'CSize'@.
foreign import ccall unsafe "hs_memzero.h &hs_memzero_finalizerEnvFree"
  finalizerEnvFree :: FP.FinalizerEnvPtr CSize a

-- | This 'FP.FinalizerEnvPtr' zeroes 'CSize' bytes starting at @'Ptr' a@.
--
-- Contrary to 'finalizerEnvFree', this /doesn't/ 'A.free' the @'Ptr' 'CSize'@.
foreign import ccall unsafe "hs_memzero.h &hs_memzero_finalizerEnv"
  finalizerEnv :: FP.FinalizerEnvPtr CSize a

