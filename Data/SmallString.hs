{-# LANGUAGE BangPatterns #-}

{-

 The SmallString type is for storing small identifiers. We do not provide fast operations
 on strings - what we offer is low memory overead.

 The Ord instance is not garanteed to be the same as that of the corresponding
 string.

 -}

module Data.SmallString
    ( SmallString
    , fromString
    , toString
    ) where

import Control.Monad.ST (runST)
import qualified Data.Primitive.ByteArray as A
import Data.Primitive.Addr()
import qualified Codec.Binary.UTF8.String as UTF8
import System.IO.Unsafe (unsafePerformIO)
import Data.Word (Word8)
import Control.DeepSeq

-- | A space efficient representation of text.
data SmallString = SmallString
           {-# UNPACK #-} !Int --  length in bytes
           {-# UNPACK #-} !A.ByteArray -- byte array

instance Eq SmallString where
    (==) = eqSmallString

instance Ord SmallString where
    compare = compareSmallString

instance Show SmallString where
    show = show . toString

instance NFData SmallString


compareSmallString :: SmallString -> SmallString -> Ordering
compareSmallString (SmallString lhsLen lhsAry) (SmallString rhsLen rhsAry)
    = go (lhsLen-1) (rhsLen-1)
 where
   go (-1) (-1)    = EQ
   go (-1) _       = GT
   go _ (-1)       = LT
   go lhsN rhsN
       = case A.indexByteArray lhsAry lhsN `compare` (A.indexByteArray rhsAry rhsN :: Word8) of
           EQ -> go (lhsN-1) (rhsN-1)
           x  -> x


eqSmallString :: SmallString -> SmallString -> Bool
eqSmallString lhsT@(SmallString lenL lhsA) rhsT@(SmallString lenR rhsA)
    | lenL /= lenR
        = False
    | otherwise = lhsT `compare` rhsT == EQ

-- | Convert a String into a SmallString.
fromString :: String -> SmallString
fromString string
    = SmallString wordLen $ runST $ do
        ary <- A.newByteArray wordLen
        mapM (uncurry $ A.writeByteArray ary) (zip [0..] wordList)
        A.unsafeFreezeByteArray ary
 where
   wordLen = length wordList
   wordList = UTF8.encode string -- I probably need some UTF normalizaion here

-- | Convert a SmallString into a String.
toString :: SmallString -> String
toString
    = UTF8.decode . toBytes

toBytes :: SmallString -> [Word8]
toBytes (SmallString len ary)
    = map (A.indexByteArray ary) [0 .. (len - 1)]


