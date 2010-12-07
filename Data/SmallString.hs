{-# LANGUAGE BangPatterns #-}

{-|

 An immutable Unicode text type, optimized for low memory overhead.  A
 'SmallString' typically uses less memory than the corresponding 'T.Text'.  Use
 'SmallText' when storing a large number of short texts, for example when
 indexing a map using words or short phrases.

 To manipulate a 'SmallString', first convert it into a 'T.Text'.  For more
 information on working with 'T.Text', see the @text@ package:
 http://hackage.haskell.org/package/text

 The Ord instance is not guaranteed to be the same as that of the corresponding
 string.

 -}

module Data.SmallString
    ( SmallString
    , fromString
    , toString
    , fromText
    , toText
    ) where

import qualified Data.SmallArray as A
import qualified Data.SmallArray.Unsafe as A

import qualified Codec.Binary.UTF8.String as UTF8
import Data.Word (Word8)
import qualified Data.String as S ( IsString(..) )

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B

import Control.DeepSeq
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (poke)

-- | A space efficient representation of Unicode text.
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

-- | Convert a 'String' into a 'SmallString'.
fromString :: String -> SmallString
fromString
    = SmallString . A.fromList . UTF8.encode

-- | Convert a 'SmallString' into a 'String'.
toString :: SmallString -> String
toString (SmallString ary)
    = UTF8.decode . A.toList $ ary

-- | Convert a 'T.Text' into a 'SmallString'.
fromText :: T.Text -> SmallString
fromText = fromBS . T.encodeUtf8

-- | Convert a 'SmallString' into a 'T.Text'.
toText :: SmallString -> T.Text
toText = T.decodeUtf8 . toBS

-- | Convert a 'B.ByteString' into a 'SmallString', assuming that the
-- 'B.ByteString' contains UTF-8 encoded text.  This assumption is not checked.
fromBS :: B.ByteString -> SmallString
fromBS bs = SmallString $
    let len = B.length bs
    in A.run $ do
      arr <- A.unsafeNew len
      mapM_ (\ix -> A.unsafeWrite arr ix (B.unsafeIndex bs ix)) [0 .. len]
      return arr

-- | Convert a 'SmallString' into a 'B.ByteString'.  The 'B.ByteString' will
-- contain UTF-8 encoded text.
toBS :: SmallString -> B.ByteString
toBS (SmallString ary) = B.unsafeCreate len (go ary 0)
  where
    len = A.length ary
    go :: A.Array Word8 -> Int -> Ptr Word8 -> IO ()
    go !ary !i !p
        | i < len = do poke p (A.unsafeIndex ary i)
                       go ary (i + 1) (p `plusPtr` 1)
        | otherwise = return ()
