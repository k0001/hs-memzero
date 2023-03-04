module Main (main) where

import Control.Exception.Safe qualified as Ex
import Control.Monad.IO.Class
import Control.Monad.Trans.Identity
import Data.Foldable
import Data.IORef
import Data.Word
import Foreign.Marshal.Alloc qualified as A (free)
import Foreign.Marshal.Utils qualified as A (new)
import Foreign.C.Types
import Foreign.Ptr
import GHC.ForeignPtr hiding (withForeignPtr)
import Foreign.Storable
import Memzero qualified as M


withForeignPtr
  :: forall p a m
  .  (MonadIO m, Ex.MonadMask m)
  => ForeignPtr p
  -> (Ptr p -> m a)
  -> m a
withForeignPtr fp g = do
  a <- g (unsafeForeignPtrToPtr fp)
  liftIO $ touchForeignPtr fp
  pure a

fill :: forall a m. MonadIO m => Ptr a -> Int -> m ()
fill = \p n -> liftIO $
    if n >= 0 then go p n else fail "fill: negative size"
  where
    go _ 0 = pure ()
    go p n = do poke (castPtr p) (fromIntegral (n `mod` 256) :: Word8)
                go (plusPtr p 1) (n - 1)

check :: MonadIO m => String -> Ptr a -> Int -> m ()
check s = \p n -> liftIO $
    if n >= 0 then go p n else fail (s <> ": check: negative size")
  where
    go _ 0 = pure ()
    go p n = do w <- peek (castPtr p)
                case w :: Word8 of
                  0 -> go (plusPtr p 1) (n - 1)
                  _ -> fail (s <> ": check: unexpected non-zero byte")

-- TODO: For some reason I have to do this. "wrapper" was leading to
-- re-entry errors, even with the foreign import marked 'safe'
foreign import ccall unsafe "test.hs.h &test_finalizerPtrEnvSet1"
  finalizerEnvSet1 :: FinalizerEnvPtr CInt p

withFinalizerIO
  :: forall a p m
  .  (MonadIO m, Ex.MonadMask m)
  => String
  -> ((ForeignPtr p -> IO ()) -> m a)
  -> m a
withFinalizerIO s f = do
  ior <- liftIO $ newIORef False
  a <- f $ flip addForeignPtrConcFinalizer (writeIORef ior True)
  liftIO $ readIORef ior >>= \case
    True  -> pure a
    False -> fail (s <> ": withFinalizerIO: not finalized")

withFinalizerC
  :: forall a p m
  .  (MonadIO m, Ex.MonadMask m)
  => String
  -> ((ForeignPtr p -> IO ()) -> m a)
  -> m a
withFinalizerC s f = Ex.bracket
  (liftIO (A.new 0))
  (liftIO . A.free)
  (\penv -> do a <- f $ addForeignPtrFinalizerEnv finalizerEnvSet1 penv
               liftIO $ peek penv >>= \case
                 1 -> pure a
                 _ -> fail (s <> ": withFinalizeC: not finalized"))

sizeOfWord :: Int
sizeOfWord = sizeOf (undefined :: Word)

main :: IO ()
main = runIdentityT $ do -- IdentityT to ensure MonadIO and MonadMask support

  do p <- M.alloca @Word $ \p -> p <$ fill p sizeOfWord
     check "alloca" p sizeOfWord

  do withFinalizerC "mallocForeignPtr" $ \addFin -> do
       fp <- M.mallocForeignPtr @Word
       liftIO $ addFin fp
       p <- withForeignPtr fp $ \p -> p <$ fill p sizeOfWord
       liftIO $ finalizeForeignPtr fp
       check "mallocForeignPtr" p sizeOfWord

  do withFinalizerIO "mallocForeignPtrConc" $ \addFin -> do
       fp <- M.mallocForeignPtrConc @Word
       liftIO $ addFin fp
       p <- withForeignPtr fp $ \p -> p <$ fill p sizeOfWord
       liftIO $ finalizeForeignPtr fp
       check "mallocForeignPtrConc" p sizeOfWord

  for_ [0..4096] $ \size -> do

    do p <- M.allocaBytes size $ \p -> p <$ fill p size
       check "allocaBytes" p size

    do p <- M.allocaBytesAligned size 16 $ \p -> p <$ fill p size
       check "allocaBytesAligned" p size

    do p <- M.allocaBytes size $ \p -> p <$ fill p size
       check "allocaBytes" p size

    do p <- M.allocaBytesAligned size 16 $ \p -> p <$ fill p size
       check "allocaBytesAligned" p size

    do withFinalizerC "mallocForeignPtrBytes" $ \addFin -> do
         fp <- M.mallocForeignPtrBytes size
         liftIO $ addFin fp
         p <- withForeignPtr fp $ \p -> p <$ fill p size
         liftIO $ finalizeForeignPtr fp
         check "mallocForeignPtrBytes" p size

    do withFinalizerC "mallocForeignPtrAlignedBytes" $ \addFin -> do
         fp <- M.mallocForeignPtrAlignedBytes size 16
         liftIO $ addFin fp
         p <- withForeignPtr fp $ \p -> p <$ fill p size
         liftIO $ finalizeForeignPtr fp
         check "mallocForeignPtrAlignedBytes" p size

    do withFinalizerIO "mallocForeignPtrConcBytes" $ \addFin -> do
         fp <- M.mallocForeignPtrConcBytes size
         liftIO $ addFin fp
         p <- withForeignPtr fp $ \p -> p <$ fill p size
         liftIO $ finalizeForeignPtr fp
         check "mallocForeignPtrConcBytes" p size

    do withFinalizerIO "mallocForeignPtrConcAlignedBytes" $ \addFin -> do
         fp <- M.mallocForeignPtrConcAlignedBytes size 16
         liftIO $ addFin fp
         p <- withForeignPtr fp $ \p -> p <$ fill p size
         liftIO $ finalizeForeignPtr fp
         check "mallocForeignPtrConcAlignedBytes" p size

  liftIO $ putStrLn "ok"


