{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

import Control.Applicative
import Data.Attoparsec.Text
import Data.Char (isDigit)
import Data.Foldable (foldl')
import Data.List
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text.IO qualified as T
import Debug.Trace
import GHC.Generics (Generic)
import Test.QuickCheck qualified as QC

type Time = Int
type Times = [Time]

type Distance = Int
type Distances = [Distance]

numbers :: Parser [Int]
numbers = many1' (skipSpace >> decimal)

times :: Parser Times
times = "Time:" >> numbers

distances :: Parser Distances
distances = "Distance:" >> numbers

data Race = Race
    { duration :: Time
    , record :: Distance
    }
    deriving stock (Show)

races :: Parser [Race]
races = do
    ts <- times
    char '\n'
    ds <- distances
    pure $ zipWith Race ts ds

beatsRecord :: Race -> Time -> Bool
beatsRecord race secondsHeld =
  let distance = (race.duration - secondsHeld) * secondsHeld
   in distance > race.record

part1 :: [Race] -> Int
part1 rs =
    product $ fmap strategies rs
  where
    strategies race =
      length $ filter (beatsRecord race) [1 .. race.duration - 1]

main :: IO ()
main = do
    input <- T.readFile "input"
    let Right rs = parseOnly races input
    print $ part1 rs
