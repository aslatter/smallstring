
{-

The aim is that SmallString should be competitive with
String for comparison operations.

-}

import Data.SmallString as SS
import Criterion.Main

import qualified Data.Set as Set
import Data.List (nub)

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B

getStrings :: IO [String]
getStrings = lines `fmap` readFile "words.txt"

getSmallStrings :: IO [SmallString]
getSmallStrings =  map SS.fromString `fmap` getStrings

getTexts :: IO [T.Text]
getTexts = map T.pack `fmap` getStrings

main = do
  strings <- getStrings
  smallStrings <- getSmallStrings
  texts <- getTexts

  let testSmallString = fromString "hello, world!"

  defaultMain
    [ bench "building set of strings" $
            nf Set.fromList strings
    , bench "building a set of texts" $
            nf Set.fromList texts
    , bench "building set of small strings" $
            nf Set.fromList smallStrings

    , bench "nub on strings" $
            nf nub strings
    , bench "nub on texts" $
            nf nub texts
    , bench "nub on small strings" $
            nf nub smallStrings

    , bench "fromString \"hello, world!\"" $
            nf fromString "hello, world!"
    , bench "toString \"hello, world!\"" $
            nf toString testSmallString

    , bgroup "conversions"
            [ bench "fromText" $ nf fromText testText
            ]
    ]

str = "the quick brown fox jumped over the lazy dog"

testText :: T.Text
testText = T.pack str
