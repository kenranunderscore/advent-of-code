{-# LANGUAGE ScopedTypeVariables #-}

import Data.Function ((&))

main :: IO ()
main = do
  rows <- lines <$> readFile "./data/day01"
  let xs :: [Int] = read <$> rows
  putStr "Number of simple increases: "
  zip xs (tail xs) & filter (uncurry (<)) & length & print
  putStr "Number of three-measurement-sum increases: "
  let sums = map (\(a, b, c) -> a + b + c) $ zip3 xs (tail xs) (tail (tail xs))
      pairs = zip sums (tail sums)
  pairs & filter (uncurry (<)) & length & print
