{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE LambdaCase #-}

import Data.List
import Control.Monad
import Data.Char
import Data.Maybe qualified as Maybe
import Data.Function ((&))
import Data.Map.Strict qualified as Map

data Color = Red | Green | Blue
  deriving (Show, Ord, Enum, Eq)

data SetOfCubes = SetOfCubes
  { red :: Int
  , green :: Int
  , blue :: Int
  }
  deriving Show

data Game = Game
  { number :: Int
  , samples :: [SetOfCubes]
  }
  deriving Show

splitList :: Eq a => a -> [a] -> [[a]]
splitList separator =
  filter (not . null)
  . (\(currentPart, parts) -> currentPart:parts)
  . foldr
      (\char (currentPart, parts) ->
         if char == separator
         then ([], currentPart:parts)
         else (char:currentPart, parts))
      ([], [])

readSetOfCubes :: String -> SetOfCubes
readSetOfCubes s =
  SetOfCubes { red = Maybe.fromMaybe 0 (Map.lookup Red m)
             , green = Maybe.fromMaybe 0 (Map.lookup Green m)
             , blue = Maybe.fromMaybe 0 (Map.lookup Blue m)
             }
  where
    m = Map.fromList $ fmap readColorInfo $ splitList ',' s

readColorInfo :: String -> (Color, Int)
readColorInfo s =
  let [n, rawColor] = splitList ' ' s
      readColor = \case
        "red" -> Red
        "green" -> Green
        "blue" -> Blue
  in (readColor rawColor, read n)

readGame :: String -> Game
readGame line =
  let [game, rawSets] = splitList ':' line
      n :: Int = read $ filter isDigit game
      samples = fmap readSetOfCubes $ splitList ';' rawSets
  in Game n samples

isPossible :: Game -> Bool
isPossible game =
  all
    (\sample ->
       sample.red <= 12 && sample.green <= 13 && sample.blue <= 14)
    game.samples

part1 :: [String] -> Int
part1 =
  sum
  . fmap (.number)
  . filter isPossible
  . fmap readGame

main :: IO ()
main = do
  input <- lines <$> readFile "input"
  print $ part1 input
