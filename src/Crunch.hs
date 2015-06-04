{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE RankNTypes          #-}

{-| Example usage for files:

>>> import Crunch
>>> encodeFile "test.dat" (1 :: Int, False)
>>> decodeFile "test.dat" :: IO (Int, Bool)
(1, False)

    Example usage for sockets:

>>> -- Launch a server in one window
>>> import Crunch
>>> import Network.Simple.TCP
>>> :set -XOverloadedStrings
>>> let debug a = print (a :: [Int])
>>> serve "127.0.0.1" "8000" (\(socket, _) -> decodeSocket socket >>= f)

>>> -- Connect to the server in another window
>>> import Crunch
>>> import Network.Simple.TCP
>>> connect "127.0.0.1" "8000" (\(socket, _) -> encodeSocket socket ([1..10] :: [Int]))

    The server will print @[1,2,3,4,5,6,7,8,9,10]@
-}

module Crunch (
    -- * Handle operations
      encodeFile
    , decodeFile
    , encodeHandle
    , decodeHandle
    , encodeSocket
    , decodeSocket

    -- * Serializable
    , Get
    , Put
    , Serializable(..)
    ) where

import Control.Applicative (Applicative(..))
import Control.Exception (bracketOnError)
import Control.Monad (replicateM)
import Data.Array (Ix)
import qualified Data.Array as A
import qualified Data.ByteString      as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Unsafe as Unsafe
import Data.Foldable (toList)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree (Tree(..))
import Data.Vector.Unboxed (Unbox)
import qualified Data.Vector          as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed  as VU
import qualified Foreign.Safe as Foreign
import Foreign.Safe (Storable(..))
import Network.Socket (Socket)
import Network.Socket.ByteString as Socket
import qualified System.IO as IO
import System.IO (Handle)

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
newtype Get a = Get { unGet :: (forall b . Ptr b -> Int -> IO Int) -> IO a }

instance Functor Get where
    fmap f (Get k) = Get (\getBuf -> fmap f (k getBuf))

instance Applicative Get where
    pure a = Get (\_ -> pure a)

    Get f <*> Get x = Get (\getBuf -> f getBuf <*> x getBuf)

instance Monad Get where
    return a = Get (\_ -> return a)

    m >>= f = Get (\getBuf -> do
        a <- unGet m getBuf
        unGet (f a) getBuf )

-- | An action that writes to a handle and returns some @a@
newtype Put a = Put { unPut :: (forall b . Ptr b -> Int -> IO ()) -> IO a }

instance Functor Put where
    fmap f (Put k) = Put (\putBuf -> fmap f (k putBuf))

instance Applicative Put where
    pure a = Put (\_ -> pure a)

    Put f <*> Put x = Put (\putBuf -> f putBuf <*> x putBuf)

instance Monad Put where
    return a = Put (\_ -> return a)

    m >>= f = Put (\putBuf -> do
        a <- unPut m putBuf
        unPut (f a) putBuf )

-- | Write a value to a file
encodeFile :: Serializable a => FilePath -> a -> IO ()
encodeFile file a = IO.withFile file IO.WriteMode (\handle -> encodeHandle handle a)

-- | Read a value from a file
decodeFile :: Serializable a => FilePath -> IO a
decodeFile file = IO.withFile file IO.ReadMode decodeHandle

-- | Write a value to a `Handle`
encodeHandle :: Serializable a => Handle -> a -> IO ()
encodeHandle handle a = unPut (put a) (IO.hPutBuf handle)

-- | Read a value from a `Handle`
decodeHandle :: Serializable a => Handle -> IO a
decodeHandle handle = unGet get (IO.hGetBuf handle)

-- | Write a value to a `Socket`
encodeSocket :: Serializable a => Socket -> a -> IO ()
encodeSocket socket a = unPut (put a) (\pointer numBytes -> do
    bytestring <- Unsafe.unsafePackCStringFinalizer
        (Foreign.castPtr pointer)
        numBytes
        (return ())
    Socket.sendAll socket bytestring )

-- | Read a value from a `Socket`
decodeSocket :: Serializable a => Socket -> IO a
decodeSocket socket = unGet get (\pointer0 bytesRequired ->
    let loop pointer remainingBytes = do
            bytestring <- Socket.recv socket (remainingBytes `min` 4096)
            Unsafe.unsafeUseAsCStringLen bytestring (\(pointer', bytesReturned) ->
                if bytesReturned == 0
                then return (bytesRequired - remainingBytes)
                else do
                    Foreign.copyBytes pointer pointer' bytesReturned
                    if bytesReturned < remainingBytes
                        then loop (Foreign.plusPtr pointer bytesReturned)
                                  (remainingBytes - bytesReturned)
                        else return bytesRequired )
    in  loop (Foreign.castPtr pointer0) bytesRequired )

{-| A value that can be serialized and deserialized

    `Serializable` has a default implementation for `Storable` types, so if
    your type @T@ implements `Storable`, then you can automatically derive a
    `Serializable` instance by writing:

> instance Serializable T
-}
class Serializable a where
    put :: a -> Put ()
    default put :: Storable a => a -> Put ()
    put a = Put (\putBuf -> Foreign.with a (\pointer ->
        putBuf pointer (Foreign.sizeOf (undefined :: a)) ))

    get :: Get a
    default get :: Storable a => Get a
    get   = Get (\getBuf -> Foreign.alloca (\pointer -> do
        let numBytes = Foreign.sizeOf (undefined :: a)
        n <- getBuf pointer numBytes
        if n < numBytes
            then fail "Storable a => Serializable a: get - Insufficient bytes"
            else return ()
        Foreign.peek pointer ))

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
    put (a, b) = do
        put a
        put b

    get = do
        a <- get
        b <- get
        return (a, b)

instance (Serializable a, Serializable b, Serializable c) => Serializable (a, b, c) where
    put (a, b, c) = do
        put a
        put b
        put c

    get = do
        a <- get
        b <- get
        c <- get
        return (a, b, c)

instance (Serializable a, Serializable b, Serializable c, Serializable d) => Serializable (a, b, c, d) where
    put (a, b, c, d) = do
        put a
        put b
        put c
        put d

    get = do
        a <- get
        b <- get
        c <- get
        d <- get
        return (a, b, c, d)

instance (Serializable a, Serializable b, Serializable c, Serializable d, Serializable e) => Serializable (a, b, c, d, e) where
    put (a, b, c, d, e) = do
        put a
        put b
        put c
        put d
        put e

    get = do
        a <- get
        b <- get
        c <- get
        d <- get
        e <- get
        return (a, b, c, d, e)

instance (Serializable a, Serializable b, Serializable c, Serializable d, Serializable e, Serializable f) => Serializable (a, b, c, d, e, f) where
    put (a, b, c, d, e, f) = do
        put a
        put b
        put c
        put d
        put e
        put f

    get = do
        a <- get
        b <- get
        c <- get
        d <- get
        e <- get
        f <- get
        return (a, b, c, d, e, f)

instance (Serializable a, Serializable b, Serializable c, Serializable d, Serializable e, Serializable f, Serializable g) => Serializable (a, b, c, d, e, f, g) where
    put (a, b, c, d, e, f, g) = do
        put a
        put b
        put c
        put d
        put e
        put f
        put g

    get = do
        a <- get
        b <- get
        c <- get
        d <- get
        e <- get
        f <- get
        g <- get
        return (a, b, c, d, e, f, g)

instance (Serializable a, Serializable b, Serializable c, Serializable d, Serializable e, Serializable f, Serializable g, Serializable h) => Serializable (a, b, c, d, e, f, g, h) where
    put (a, b, c, d, e, f, g, h) = do
        put a
        put b
        put c
        put d
        put e
        put f
        put g
        put h

    get = do
        a <- get
        b <- get
        c <- get
        d <- get
        e <- get
        f <- get
        g <- get
        h <- get
        return (a, b, c, d, e, f, g, h)

instance (Serializable a, Serializable b, Serializable c, Serializable d, Serializable e, Serializable f, Serializable g, Serializable h, Serializable i) => Serializable (a, b, c, d, e, f, g, h, i) where
    put (a, b, c, d, e, f, g, h, i) = do
        put a
        put b
        put c
        put d
        put e
        put f
        put g
        put h
        put i

    get = do
        a <- get
        b <- get
        c <- get
        d <- get
        e <- get
        f <- get
        g <- get
        h <- get
        i <- get
        return (a, b, c, d, e, f, g, h, i)

instance Serializable a => Serializable [a] where
    put as = case as of
        [] -> put (0 :: Int)
        _  -> do
            let chunkSize = 100 :: Int
                (prefix, suffix) = splitAt chunkSize as
            put (case suffix of
                [] -> length prefix
                _  -> chunkSize )
            mapM_ put prefix
            put suffix

    get = do
        n <- get :: Get Int
        case n of
            0 -> return []
            _ -> do
                prefix <- replicateM (fromIntegral n) get
                fmap (prefix ++) get

instance Storable a => Serializable (VS.Vector a) where
    put v = do
        let numElements = VS.length v
            elementSize = Foreign.sizeOf (undefined :: a)
        put numElements
        Put (\putBuf -> VS.unsafeWith v (\pointer ->
            putBuf pointer (numElements * elementSize) ))

    get = do
        numElements <- get
        let elementSize = Foreign.sizeOf (undefined :: a)
        Get (\getBuf -> do
            foreignPointer <- Foreign.mallocForeignPtrArray numElements
            let numBytes = numElements * elementSize
            n <- Foreign.withForeignPtr foreignPointer (\pointer ->
                getBuf pointer numBytes )
            if n < numBytes
                then fail "Storable a => Serializable a: get - Insufficient bytes"
                else return ()
            return (VS.unsafeFromForeignPtr0 foreignPointer numElements))

instance Serializable a => Serializable (V.Vector a) where
    put v = do
        put (V.length v)
        -- Equivalent to `V.mapM_ put v`, but faster due to specialization
        Put (\putBuf -> V.mapM_ (\a -> unPut (put a) putBuf) v)

    get = do
        n  <- get
        -- Equivalent to `V.replicateM n get`, but faster due to specialization
        Get (\getBuf -> V.replicateM n (unGet get getBuf))

instance Serializable Strict.ByteString where
    put bytestring = Put (\putBuf -> do
        Unsafe.unsafeUseAsCStringLen bytestring (\(pointer, numBytes) -> do
            unPut (put numBytes) putBuf
            putBuf pointer numBytes ) )

    get = Get (\getBuf -> do
        numBytes <- Foreign.alloca (\pointer -> do
            let intSize = Foreign.sizeOf (undefined :: Int)
            n <- getBuf pointer intSize
            if n < intSize
                then fail "Serializable ByteString: get - Insufficient bytes"
                else return ()
            Foreign.peek pointer )
        bracketOnError (Foreign.mallocBytes numBytes) Foreign.free (\pointer -> do
            n <- getBuf pointer numBytes
            if n < numBytes
                then fail "Serializable ByteString: get - Insufficient bytes"
                else return ()
            Unsafe.unsafePackCStringFinalizer
                pointer
                numBytes
                (Foreign.free pointer) ) )

instance Serializable Lazy.ByteString where
    put bytestring = put (Lazy.toChunks bytestring)

    get = fmap Lazy.fromChunks get

instance (Ix i, Serializable i, Serializable a) => Serializable (A.Array i a) where
    put arr = put (A.bounds arr, A.elems arr)
    get = fmap (uncurry A.listArray) get

instance Serializable a => Serializable (Set a) where
    put set = put (Set.toAscList set)

    get = fmap Set.fromDistinctAscList get

instance (Serializable k, Serializable v) => Serializable (Map k v) where
    put m = put (Map.toAscList m)

    get = fmap Map.fromDistinctAscList get

instance Serializable a => Serializable (IntMap a) where
    put m = put (IntMap.toAscList m)

    get = fmap IntMap.fromDistinctAscList get

instance Serializable a => Serializable (Seq a) where
    put s = put (toList s)

    get = fmap Seq.fromList get

instance Serializable a => Serializable (Tree a) where
    put (Node a ts) = do
        put a
        put ts

    get = do
        a  <- get
        ts <- get
        return (Node a ts)

instance (Unbox a, Serializable a) => Serializable (VU.Vector a) where
    put v = put (VU.convert v :: V.Vector a)

    get = fmap VU.convert (get :: Get (V.Vector a))
