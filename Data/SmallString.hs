{-# LANGUAGE BangPatterns #-}

{-|

 The SmallString type is for storing small identifiers. We do not provide fast operations
 on strings - what we offer is low memory overhead.

 The Ord instance is not garaunteed to be the same as that of the corresponding
 string.

 Currently SmallStrings use NULL charecters as sentinels. This means that:

 (toString . fromString) "hello\NULLworlds" == "hello"

 -}

module Data.SmallString
    ( SmallString
    , fromString
    , toString
    ) where

import qualified Data.ByteArray as A
import qualified Codec.Binary.UTF8.String as UTF8
import System.IO.Unsafe (unsafePerformIO)
import Data.Word (Word8)
import Control.DeepSeq
import Foreign (unsafePerformIO)

-- | A space efficient representation of text. This is like a strict ByteString, but
-- with fewer features.
newtype SmallString = SmallString A.ByteArray

instance Eq SmallString where
    (==) = eqSmallString

instance Ord SmallString where
    compare = compareSmallString

instance Show SmallString where
    show = show . toString

instance NFData SmallString

compareSmallString :: SmallString -> SmallString -> Ordering
compareSmallString (SmallString lhsAry) (SmallString rhsAry)
    = go 0
 where
   go n
       = case (A.indexByteArray lhsAry n :: Word8, A.indexByteArray rhsAry n) of
           (0,0) -> EQ
           (0,_) -> GT
           (_,0) -> LT
           (l,r) -> case l `compare` r of
                     EQ -> go (n+1)
                     x  -> x

eqSmallString :: SmallString -> SmallString -> Bool
eqSmallString lhs rhs
    = lhs `compare` rhs == EQ


-- | Convert a String into a SmallString.
fromString :: String -> SmallString
fromString string
    = SmallString $ unsafePerformIO $ do
        ary <- A.newByteArray (wordLen+1)
        mapM_ (uncurry $ A.writeByteArray ary) (zip [0..(wordLen-1)] wordList)
        A.writeByteArray ary wordLen (0 :: Word8)
        A.unsafeFreezeByteArray ary
 where
   wordLen = length wordList
   wordList = UTF8.encode string

-- | Convert a SmallString into a String.
toString :: SmallString -> String
toString (SmallString ary)
    = UTF8.decode $ go 0

 where go n = case A.indexByteArray ary n of
                0 -> []
                x -> x : go (n+1)



