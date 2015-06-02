{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DefaultSignatures          #-}

module Crunch (
    -- * Handle operations
      encodeFile
    , decodeFile

    -- * Serializable
    , Get
    , Put
    , Serializable(..)
    ) where

import Control.Applicative (Applicative)
import Control.Monad (replicateM)
import Control.Monad.Trans.Reader
import qualified Data.Vector          as V
import qualified Data.Vector.Storable as VS
import qualified System.IO as IO
import qualified Foreign.Safe as Foreign
import Foreign (Storable(..))

import Data.Word (Word, Word8, Word16, Word32, Word64)
import Data.Int  (       Int8,  Int16,  Int32,  Int64)
import Foreign.C.Types
    ( CUIntMax
    , CIntMax
    , CUIntPtr
    , CIntPtr
    , CSUSeconds
    , CUSeconds
    , CTime
    , CClock
    , CSigAtomic
    , CWchar
    , CSize
    , CPtrdiff
    , CDouble
    , CFloat
    , CULLong
    , CLLong
    , CULong
    , CLong
    , CUInt
    , CInt
    , CUShort
    , CShort
    , CUChar
    , CSChar
    , CChar
    )
import Foreign.Ptr (IntPtr, WordPtr, FunPtr, Ptr)
import Foreign.StablePtr (StablePtr)
import GHC.Fingerprint.Type (Fingerprint)
import System.Posix.Types
    ( Fd
    , CRLim
    , CTcflag
    , CSpeed
    , CCc
    , CUid
    , CNlink
    , CGid
    , CSsize
    , CPid
    , COff
    , CMode
    , CIno
    , CDev
    )

-- TODO: Implement `ReaderT` inline

newtype Get a = Get { unGet :: ReaderT IO.Handle IO a }
    deriving (Functor, Applicative, Monad)

newtype Put a = Put { unPut :: ReaderT IO.Handle IO a }
    deriving (Functor, Applicative, Monad)

encodeFile :: Serializable a => FilePath -> a -> IO ()
encodeFile file a = IO.withFile file IO.WriteMode (runReaderT (unPut (put a)))

decodeFile :: Serializable a => FilePath -> IO a
decodeFile file = IO.withFile file IO.ReadMode (runReaderT (unGet get))

class Serializable a where
    get :: Get a
    default get :: Storable a => Get a
    get   = Get (ReaderT (\handle -> Foreign.alloca (\pointer -> do
        let numBytes = Foreign.sizeOf (undefined :: a)
        n <- IO.hGetBuf handle pointer (Foreign.sizeOf numBytes)
        if n < numBytes
            then fail "Storable a => Serializable a: get - Insufficient bytes"
            else return ()
        Foreign.peek pointer )))

    put :: a -> Put ()
    default put :: Storable a => a -> Put ()
    put a = Put (ReaderT (\handle -> do
        Foreign.with a (\pointer ->
            IO.hPutBuf handle pointer (Foreign.sizeOf (undefined :: a)) ) ))

instance Serializable Bool
instance Serializable Char
instance Serializable Double
instance Serializable Float
instance Serializable Int
instance Serializable Int8
instance Serializable Int16
instance Serializable Int32
instance Serializable Int64
instance Serializable Word
instance Serializable Word8
instance Serializable Word16
instance Serializable Word32
instance Serializable Word64
instance Serializable Fingerprint
instance Serializable CUIntMax
instance Serializable CIntMax
instance Serializable CUIntPtr
instance Serializable CIntPtr
instance Serializable CSUSeconds
instance Serializable CUSeconds
instance Serializable CTime
instance Serializable CClock
instance Serializable CSigAtomic
instance Serializable CWchar
instance Serializable CSize
instance Serializable CPtrdiff
instance Serializable CDouble
instance Serializable CFloat
instance Serializable CULLong
instance Serializable CLLong
instance Serializable CULong
instance Serializable CLong
instance Serializable CUInt
instance Serializable CInt
instance Serializable CUShort
instance Serializable CShort
instance Serializable CUChar
instance Serializable CSChar
instance Serializable CChar
instance Serializable IntPtr
instance Serializable WordPtr
instance Serializable Fd
instance Serializable CRLim
instance Serializable CTcflag
instance Serializable CSpeed
instance Serializable CCc
instance Serializable CUid
instance Serializable CNlink
instance Serializable CGid
instance Serializable CSsize
instance Serializable CPid
instance Serializable COff
instance Serializable CMode
instance Serializable CIno
instance Serializable CDev
instance Serializable (StablePtr a)
instance Serializable (Ptr a)
instance Serializable (FunPtr a)

instance (Serializable a, Serializable b) => Serializable (a, b) where
    get = do
        a <- get
        b <- get
        return (a, b)

    put (a, b) = do
        put a
        put b

instance (Serializable a, Serializable b, Serializable c) => Serializable (a, b, c) where
    get = do
        a <- get
        b <- get
        c <- get
        return (a, b, c)

    put (a, b, c) = do
        put a
        put b
        put c

instance Serializable a => Serializable [a] where
    get = do
        b <- get :: Get Word8
        case b of
            0 -> return []
            1 -> do
                n      <- get :: Get Int
                prefix <- replicateM n get
                fmap (prefix ++) get
            _ -> fail "Storable a => Serializable [a]: get - Invalid tag byte"
    put as = case as of
        [] -> put (0 :: Word8)
        _  -> do
            put (1 :: Word8)
            let chunkSize = 100 :: Int
                (prefix, suffix) = splitAt chunkSize as
            case suffix of
                [] -> put (length prefix)
                _  -> put  chunkSize
            mapM_ put prefix
            put suffix

instance Storable a => Serializable (VS.Vector a) where
    get = do
        numElements <- get
        let elementSize = Foreign.sizeOf (undefined :: a)
        Get (ReaderT (\handle -> do
            foreignPointer <- Foreign.mallocForeignPtrArray numElements
            let numBytes = numElements * elementSize
            n <- Foreign.withForeignPtr foreignPointer (\pointer ->
                IO.hGetBuf handle pointer numBytes )
            if n < numBytes
                then fail "Storable a => Serializable a: get - Insufficient bytes"
                else return ()
            return (VS.unsafeFromForeignPtr0 foreignPointer numElements)))

    put v = do
        let numElements = VS.length v
            elementSize = Foreign.sizeOf (undefined :: a)
        put numElements
        Put (ReaderT (\handle -> VS.unsafeWith v (\pointer ->
            IO.hPutBuf handle pointer (numElements * elementSize) )))

instance Serializable a => Serializable (V.Vector a) where
    get = do
        n  <- get
        -- Equivalent to `V.replicateM n get`, but faster due to specialization
        Get (ReaderT (\handle ->
            V.replicateM n (runReaderT (unGet get) handle) ))

    put v = do
        put (V.length v)
        -- Equivalent to `V.mapM_ put v`, but faster due to specialization
        Put (ReaderT (\handle ->
            V.mapM_ (\a -> runReaderT (unPut (put a)) handle) v ))
