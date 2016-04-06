{-# LANGUAGE CPP #-}

module SC.Data.IORef
    (
        -- * IORefs
        IORef,                -- abstract, instance of: Eq, Typeable
        newIORef,
        readIORef,
        writeIORef,
        modifyIORef,
        modifyIORef',
        atomicModifyIORef,
        atomicModifyIORef',
        atomicWriteIORef,

#if !defined(__PARALLEL_HASKELL__)
        mkWeakIORef,
#endif
        ) where


-- import GHC.Base
-- import GHC.STRef
-- import GHC.IORef hiding (atomicModifyIORef)
-- import qualified GHC.IORef
#if !defined(__PARALLEL_HASKELL__)
import GHC.Weak
#endif
import SC.Data.IORef.Unsafe (IORef(..), newIORef, readIORef)
import qualified SC.Data.IORef.Unsafe as U

modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef = U.modifyIORef
modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' = U.modifyIORef'
atomicModifyIORef :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORef = U.atomicModifyIORef
atomicModifyIORef' :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORef' = U.atomicModifyIORef'
writeIORef  :: IORef a -> a -> IO ()
writeIORef = U.atomicWriteIORef
atomicWriteIORef :: IORef a -> a -> IO ()
atomicWriteIORef = U.atomicWriteIORef
                   
#if !defined(__PARALLEL_HASKELL__)
mkWeakIORef :: IORef a -> IO () -> IO (Weak (IORef a))
mkWeakIORef = U.mkWeakIORef
#endif
