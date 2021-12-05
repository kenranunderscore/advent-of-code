{-# LANGUAGE ScopedTypeVariables #-}

module Day01 where

import Data.Function ((&))

main :: IO ()
main = do
  rows <- lines <$> readFile "./data/day01"
  let xs :: [Int] = read <$> rows
  putStr "Number of simple increases: "
  print $ numberOfIncreases xs
  putStr "Number of three-measurement-sum increases: "
  print $ numberOfThreeMeasurementIncreases xs

numberOfIncreases :: [Int] -> Int
numberOfIncreases xs =
  length $
    filter (uncurry (<)) $
      zip xs (tail xs)

numberOfThreeMeasurementIncreases :: [Int] -> Int
numberOfThreeMeasurementIncreases xs =
  let sums = map (\(a, b, c) -> a + b + c) $ zip3 xs (tail xs) (tail (tail xs))
      pairs = zip sums (tail sums)
   in length $ filter (uncurry (<)) pairs
