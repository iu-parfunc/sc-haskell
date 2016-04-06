-- {-# LANGUAGE Safe #-} -- is it?
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

-- #if !defined(__PARALLEL_HASKELL__)
--         mkWeakIORef,
-- #endif
        ) where


-- import GHC.Base
-- import GHC.STRef
-- import GHC.IORef hiding (atomicModifyIORef)
-- import qualified GHC.IORef
-- #if !defined(__PARALLEL_HASKELL__)
-- import GHC.Weak
-- #endif
import SC.Data.IORef.Unsafe (IORef(..), newIORef, readIORef,
                             atomicModifyIORef, atomicModifyIORef',
                             atomicWriteIORef)
import qualified SC.Data.IORef.Unsafe as U


-- Are we going to provide alternate, safer implementations of these?
modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef = U.modifyIORef
modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' = U.modifyIORef'
writeIORef  :: IORef a -> a -> IO ()
writeIORef = U.atomicWriteIORef
                   
-- #if !defined(__PARALLEL_HASKELL__)
-- mkWeakIORef :: IORef a -> IO () -> IO (Weak (IORef a))
-- mkWeakIORef = U.mkWeakIORef
-- #endif
