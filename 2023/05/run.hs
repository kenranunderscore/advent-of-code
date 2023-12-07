{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

import Data.Attoparsec.Text
import Data.Foldable (foldl')
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text.IO qualified as T

type Seeds = [Int]

seeds :: Parser Seeds
seeds = do
    string "seeds:"
    many' seed
  where
    seed = char ' ' *> decimal

data Range = Range
    { dest :: Int
    , from :: Int
    , length :: Int
    }
    deriving (Show)

ranges :: Text -> Parser [Range]
ranges name = do
    string name <* string " map:\n"
    many' (range <* char '\n')
  where
    range = Range <$> decimal <*> (char ' ' *> decimal) <*> (char ' ' *> decimal)

type Ranges = [Range]

type Mapping = Ranges

mapping :: Text -> Parser Mapping
mapping = ranges

seedsAndMappings :: Parser (Seeds, [Mapping])
seedsAndMappings = do
    xs <- seeds
    char '\n' *> char '\n'
    seedToSoil <- mapping "seed-to-soil"
    char '\n'
    soilToFertilizer <- mapping "soil-to-fertilizer"
    char '\n'
    fertilizerToWater <- mapping "fertilizer-to-water"
    char '\n'
    waterToLight <- mapping "water-to-light"
    char '\n'
    lightToTemperature <- mapping "light-to-temperature"
    char '\n'
    temperatureToHumidity <- mapping "temperature-to-humidity"
    char '\n'
    humidityToLocation <- mapping "humidity-to-location"
    pure
        ( xs
        ,
            [ seedToSoil
            , soilToFertilizer
            , fertilizerToWater
            , waterToLight
            , lightToTemperature
            , temperatureToHumidity
            , humidityToLocation
            ]
        )

mapSeed :: [Mapping] -> Int -> Int
mapSeed ms s =
    foldl'
        ( \i rs ->
            case mapMaybe (applyRange i) rs of
                [] -> i
                [res] -> res
                _ -> error "whyyy"
        )
        s
        ms

applyRange :: Int -> Range -> Maybe Int
applyRange i r =
    if r.from <= i && i < r.from + r.length
        then Just $ i + r.dest - r.from
        else Nothing

part1 :: Text -> Int
part1 input = do
    let Right (s, ms) = parseOnly seedsAndMappings input
    minimum $ fmap (mapSeed ms) s

main :: IO ()
main = do
    input <- T.readFile "input"
    print $ part1 input
