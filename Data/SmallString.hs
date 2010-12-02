{-# LANGUAGE BangPatterns #-}

{-|

 The SmallString type is for storing small identifiers. We do not provide fast operations
 on strings - what we offer is low memory overhead.

 The Ord instance is not garaunteed to be the same as that of the corresponding
 string.

 -}

module Data.SmallString
    ( SmallString
    , fromString
    , toString
    ) where

import qualified Data.SmallArray as A
import qualified Codec.Binary.UTF8.String as UTF8
import Data.Word (Word8)

import Control.DeepSeq

-- | A space efficient representation of text. This is like a strict ByteString, but
-- with fewer features, and UTF preserving. Fow ASCII data, we're slightly smaller than
-- ByteStrings for small strings.
newtype SmallString = SmallString (A.Array Word8)

instance Eq SmallString where
    (==) = eqSmallString

instance Ord SmallString where
    compare = compareSmallString

instance Show SmallString where
    show = show . toString

instance NFData SmallString where
    rnf (SmallString arr) = rnf arr

compareSmallString :: SmallString -> SmallString -> Ordering
compareSmallString (SmallString lhsAry) (SmallString rhsAry)
    = compare lhsAry rhsAry

eqSmallString :: SmallString -> SmallString -> Bool
eqSmallString lhs rhs
    = lhs == rhs

-- | Convert a String into a SmallString.
fromString :: String -> SmallString
fromString
    = SmallString . A.fromList . UTF8.encode

-- | Convert a SmallString into a String.
toString :: SmallString -> String
toString (SmallString ary)
    = UTF8.decode . A.toList $ ary
