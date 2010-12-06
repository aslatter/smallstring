{-# LANGUAGE BangPatterns #-}

{-|

 The SmallString type is for storing small identifiers. We do not provide fast operations
 on strings - what we offer is low memory overhead.

 The Ord instance is not guaranteed to be the same as that of the corresponding
 string.

 -}

module Data.SmallString
    ( SmallString
    , fromString
    , toString
    , fromText
    ) where

import qualified Data.SmallArray as A
import qualified Data.SmallArray.Unsafe as A

import qualified Codec.Binary.UTF8.String as UTF8
import Data.Word (Word8)
import qualified Data.String as S ( IsString(..) )

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

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

instance S.IsString SmallString where
    fromString = fromString

compareSmallString :: SmallString -> SmallString -> Ordering
compareSmallString (SmallString lhsAry) (SmallString rhsAry)
    = compare lhsAry rhsAry

eqSmallString :: SmallString -> SmallString -> Bool
eqSmallString (SmallString lhs) (SmallString rhs)
    = lhs == rhs

-- | Convert a String into a SmallString.
fromString :: String -> SmallString
fromString
    = SmallString . A.fromList . UTF8.encode

-- | Convert a SmallString into a String.
toString :: SmallString -> String
toString (SmallString ary)
    = UTF8.decode . A.toList $ ary

-- | Conver 'Text' into a SmallString
fromText :: T.Text -> SmallString
fromText = fromBS . T.encodeUtf8

fromBS :: B.ByteString -> SmallString
fromBS bs = SmallString $
    let len = B.length bs
    in A.run $ do
      arr <- A.unsafeNew len
      mapM_ (\ix -> A.unsafeWrite arr ix (B.unsafeIndex bs ix)) [0 .. len]
      return arr
