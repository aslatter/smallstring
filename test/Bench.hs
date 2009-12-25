
{-

The aim is that SmallString should be competitive with
String for comparison operations.

-}

import Data.SmallString as SS
import Criterion.Main

import qualified Data.Set as Set
import Data.List (nub)

getStrings :: IO [String]
getStrings = lines `fmap` readFile "words.txt"

getSmallStrings :: IO [SmallString]
getSmallStrings =  map SS.fromString `fmap` getStrings

main = do
  strings <- getStrings
  smallStrings <- getSmallStrings

  let testSmallString = fromString "hello, world!"

  defaultMain
    [ bench "building set of strings" $
            nf Set.fromList strings
    , bench "building set of small strings" $
            nf Set.fromList smallStrings

    , bench "nub on strings" $
            nf nub strings
    , bench "nub on small strings" $
            nf nub smallStrings

    , bench "fromString \"hello, world!\"" $
            nf fromString "hello, world!"

    , bench "toString \"hello, world!\"" $
            nf toString testSmallString
    ]

