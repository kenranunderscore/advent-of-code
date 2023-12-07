{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

import Control.Monad
import Data.Char
import Data.Function ((&))
import Data.List
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe

data Color = Red | Green | Blue
    deriving (Show, Ord, Enum, Eq)

data SetOfCubes = SetOfCubes
    { red :: Int
    , green :: Int
    , blue :: Int
    }
    deriving (Show)

data Game = Game
    { number :: Int
    , samples :: [SetOfCubes]
    }
    deriving (Show)

splitList :: Eq a => a -> [a] -> [[a]]
splitList separator =
    filter (not . null)
        . (\(currentPart, parts) -> currentPart : parts)
        . foldr
            ( \char (currentPart, parts) ->
                if char == separator
                    then ([], currentPart : parts)
                    else (char : currentPart, parts)
            )
            ([], [])

readSetOfCubes :: String -> SetOfCubes
readSetOfCubes s =
    SetOfCubes
        { red = Maybe.fromMaybe 0 (Map.lookup Red m)
        , green = Maybe.fromMaybe 0 (Map.lookup Green m)
        , blue = Maybe.fromMaybe 0 (Map.lookup Blue m)
        }
  where
    m = Map.fromList $ fmap readColorInfo $ splitList ',' s

readColorInfo :: String -> (Color, Int)
readColorInfo s =
    let
        [n, rawColor] = splitList ' ' s
        readColor = \case
            "red" -> Red
            "green" -> Green
            "blue" -> Blue
    in
        (readColor rawColor, read n)

readGame :: String -> Game
readGame line =
    let
        [game, rawSets] = splitList ':' line
        n :: Int = read $ filter isDigit game
        samples = fmap readSetOfCubes $ splitList ';' rawSets
    in
        Game n samples

isPossible :: Game -> Bool
isPossible game =
    all
        ( \sample ->
            sample.red <= 12 && sample.green <= 13 && sample.blue <= 14
        )
        game.samples

part1 :: [String] -> Int
part1 =
    sum
        . fmap (.number)
        . filter isPossible
        . fmap readGame

minimumSetOfCubes :: Game -> SetOfCubes
minimumSetOfCubes game =
    let
        redMin = maximum $ fmap (.red) game.samples
        greenMin = maximum $ fmap (.green) game.samples
        blueMin = maximum $ fmap (.blue) game.samples
    in
        SetOfCubes{red = redMin, green = greenMin, blue = blueMin}

power :: SetOfCubes -> Int
power set = set.red * set.green * set.blue

part2 :: [String] -> Int
part2 =
    sum
        . fmap power
        . fmap minimumSetOfCubes
        . fmap readGame

main :: IO ()
main = do
    input <- lines <$> readFile "input"
    print $ part1 input
    print $ part2 input
