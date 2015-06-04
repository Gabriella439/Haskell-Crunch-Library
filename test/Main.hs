{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.ByteString      as Strict
import qualified Data.ByteString.Lazy as   Lazy
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Data.Text            as Strict
import qualified Data.Text.Lazy       as   Lazy
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Data.Int  (       Int8,  Int16,  Int32,  Int64)
import Crunch (Serializable, encodeBytes, decodeBytes)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)

roundTrip :: (Eq a, Serializable a) => a -> Bool
roundTrip x = case decodeBytes (encodeBytes x) of
    Left  _  -> False
    Right x' -> x' == x

main :: IO ()
main = defaultMain
    [ testProperty "Bool"              (roundTrip :: Bool             -> Bool)
    , testProperty "Char"              (roundTrip :: Bool             -> Bool)
    , testProperty "Double"            (roundTrip :: Double           -> Bool)
    , testProperty "Float"             (roundTrip :: Float            -> Bool)
    , testProperty "Int"               (roundTrip :: Int              -> Bool)
    , testProperty "Int8"              (roundTrip :: Int8             -> Bool)
    , testProperty "Int16"             (roundTrip :: Int16            -> Bool)
    , testProperty "Int32"             (roundTrip :: Int32            -> Bool)
    , testProperty "Int64"             (roundTrip :: Int64            -> Bool)
    , testProperty "Word"              (roundTrip :: Word             -> Bool)
    , testProperty "Word8"             (roundTrip :: Word8            -> Bool)
    , testProperty "Word16"            (roundTrip :: Word16           -> Bool)
    , testProperty "Word32"            (roundTrip :: Word32           -> Bool)
    , testProperty "Word64"            (roundTrip :: Word64           -> Bool)
    , testProperty "Strict.ByteString" (roundTrip :: StrictByteString -> Bool)
    , testProperty "Lazy.ByteString"   (roundTrip :: LazyByteString   -> Bool)
    , testProperty "Strict.Text"       (roundTrip :: StrictText       -> Bool)
    , testProperty "Lazy.Text"         (roundTrip :: LazyText         -> Bool)
    , testProperty "[Int]"             (roundTrip :: [Int]            -> Bool)
    , testProperty "(Int, Int)"        (roundTrip :: (Int, Int)       -> Bool)
    ]

newtype StrictByteString = StrictByteString Strict.ByteString
    deriving (Eq, Show, Serializable)
newtype LazyByteString   = LazyByteString   Lazy.ByteString
    deriving (Eq, Show, Serializable)
newtype StrictText       = StrictText       Strict.Text
    deriving (Eq, Show, Serializable)
newtype LazyText         = LazyText         Lazy.Text
    deriving (Eq, Show, Serializable)

instance Arbitrary StrictByteString where
    arbitrary = fmap (StrictByteString . Data.ByteString.pack     ) arbitrary

instance Arbitrary LazyByteString where
    arbitrary = fmap (LazyByteString   . Data.ByteString.Lazy.pack) arbitrary

instance Arbitrary StrictText where
    arbitrary = fmap (StrictText       . Data.Text.pack           ) arbitrary

instance Arbitrary LazyText where
    arbitrary = fmap (LazyText         . Data.Text.Lazy.pack      ) arbitrary
