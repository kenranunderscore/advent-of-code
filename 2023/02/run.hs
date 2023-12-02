{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE LambdaCase #-}

import Data.List
import Control.Monad
import Data.Char
import Data.Function ((&))
import Data.Map.Strict qualified as Map

data Color = Red | Green | Blue
  deriving (Show, Ord, Enum, Eq)

type Sample = Map.Map Color Int

data Game = Game
  { number :: Int
  , samples :: [Sample]
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

readSample :: String -> Sample
readSample =
  Map.fromList . fmap readColorInfo . splitList ','

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
  let [game, rawSamples] = splitList ':' line
      n :: Int = read $ filter isDigit game
      samples = fmap readSample $ splitList ';' rawSamples
  in Game n samples

isPossible :: Game -> Bool
isPossible game =
  all
    (\sample ->
       let mred = Map.lookup Red sample
           mgreen = Map.lookup Green sample
           mblue = Map.lookup Blue sample
       -- Nothing < everything, otherwise it's the usual 'Ord'
       in mred <= Just 12 && mgreen <= Just 13 && mblue <= Just 14)
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
