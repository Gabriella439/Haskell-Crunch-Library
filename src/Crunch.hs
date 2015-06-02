{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DefaultSignatures          #-}

{-| Example usage:

>>> encodeFile "test.dat" (1 :: Int, False)
>>> decodeFile "test.dat" :: IO (Int, Bool)
(1, False)

-}

module Crunch (
    -- * Handle operations
      encodeFile
    , decodeFile

    -- * Serializable
    , Get
    , Put
    , Serializable(..)
    ) where

import Control.Applicative (Applicative(..))
import Control.Monad (replicateM)
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

-- TODO: Implementaton using GHC generics

-- | An action that reads from a handle and returns some @a@
newtype Get a = Get { unGet :: IO.Handle -> IO a }

instance Functor Get where
    fmap f (Get k) = Get (\handle -> fmap f (k handle))

instance Applicative Get where
    pure a = Get (\_ -> pure a)

    Get f <*> Get x = Get (\handle -> f handle <*> x handle)

instance Monad Get where
    return a = Get (\_ -> return a)

    m >>= f = Get (\handle -> do
        a <- unGet m handle
        unGet (f a) handle )

-- | An action that writes to a handle and returns some @a@
newtype Put a = Put { unPut :: IO.Handle -> IO a }

instance Functor Put where
    fmap f (Put k) = Put (\handle -> fmap f (k handle))

instance Applicative Put where
    pure a = Put (\_ -> pure a)

    Put f <*> Put x = Put (\handle -> f handle <*> x handle)

instance Monad Put where
    return a = Put (\_ -> return a)

    m >>= f = Put (\handle -> do
        a <- unPut m handle
        unPut (f a) handle )

-- | Write a value to a file
encodeFile :: Serializable a => FilePath -> a -> IO ()
encodeFile file a = IO.withFile file IO.WriteMode (unPut (put a))

-- | Read a value from a file
decodeFile :: Serializable a => FilePath -> IO a
decodeFile file = IO.withFile file IO.ReadMode (unGet get)

{-| A value that can be serialized and deserialized

    `Serializable` has a default implementation for `Storable` types, so if
    your type @T@ implements `Storable`, then you can automatically derive a
    `Serializable` instance by writing:

> instance Serializable T
-}
class Serializable a where
    get :: Get a
    default get :: Storable a => Get a
    get   = Get (\handle -> Foreign.alloca (\pointer -> do
        let numBytes = Foreign.sizeOf (undefined :: a)
        n <- IO.hGetBuf handle pointer (Foreign.sizeOf numBytes)
        if n < numBytes
            then fail "Storable a => Serializable a: get - Insufficient bytes"
            else return ()
        Foreign.peek pointer ))

    put :: a -> Put ()
    default put :: Storable a => a -> Put ()
    put a = Put (\handle -> Foreign.with a (\pointer ->
        IO.hPutBuf handle pointer (Foreign.sizeOf (undefined :: a)) ))

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
        n <- get :: Get Word8
        case n of
            0 -> return []
            _ -> do
                prefix <- replicateM (fromIntegral n) get
                fmap (prefix ++) get
    put as = case as of
        [] -> put (0 :: Word8)
        _  -> do
            let chunkSize = 100 :: Int
                (prefix, suffix) = splitAt chunkSize as
            put (case suffix of
                [] -> length prefix
                _  -> chunkSize )
            mapM_ put prefix
            put suffix

instance Storable a => Serializable (VS.Vector a) where
    get = do
        numElements <- get
        let elementSize = Foreign.sizeOf (undefined :: a)
        Get (\handle -> do
            foreignPointer <- Foreign.mallocForeignPtrArray numElements
            let numBytes = numElements * elementSize
            n <- Foreign.withForeignPtr foreignPointer (\pointer ->
                IO.hGetBuf handle pointer numBytes )
            if n < numBytes
                then fail "Storable a => Serializable a: get - Insufficient bytes"
                else return ()
            return (VS.unsafeFromForeignPtr0 foreignPointer numElements))

    put v = do
        let numElements = VS.length v
            elementSize = Foreign.sizeOf (undefined :: a)
        put numElements
        Put (\handle -> VS.unsafeWith v (\pointer ->
            IO.hPutBuf handle pointer (numElements * elementSize) ))

instance Serializable a => Serializable (V.Vector a) where
    get = do
        n  <- get
        -- Equivalent to `V.replicateM n get`, but faster due to specialization
        Get (\handle -> V.replicateM n (unGet get handle))

    put v = do
        put (V.length v)
        -- Equivalent to `V.mapM_ put v`, but faster due to specialization
        Put (\handle -> V.mapM_ (\a -> unPut (put a) handle) v)
