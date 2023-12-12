{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

import Control.Applicative
import Data.Attoparsec.Text
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Functor
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Debug.Trace

newtype Universe = Universe {value :: [String]}

instance Show Universe where
    show (Universe m) = List.intercalate "\n" m

imapMaybe :: (Int -> a -> Maybe b) -> [a] -> [b]
imapMaybe f = Maybe.catMaybes . zipWith f [0 ..]

emptyRows :: [String] -> [Int]
emptyRows u =
    imapMaybe (\y r -> if all (== '.') r then Just y else Nothing) u

emptyColumns :: [String] -> [Int]
emptyColumns = emptyRows . List.transpose

type Galaxy = (Int, Int)

type Scaling = Int

galaxies :: Scaling -> Universe -> [Galaxy]
galaxies scaling u =
    concat $
        imapMaybe
            ( \y row ->
                let gs =
                        imapMaybe
                            ( \x c ->
                                if c == '#'
                                    then Just (translateRow y, translateColumn x)
                                    else Nothing
                            )
                            row
                in if null gs then Nothing else Just gs
            )
            u.value
  where
    rs = emptyRows u.value
    cs = emptyColumns u.value
    translateIndex emptyIndices i = i + (scaling - 1) * length (filter (< i) emptyIndices)
    translateRow = translateIndex rs
    translateColumn = translateIndex cs

pairings :: [Galaxy] -> [(Galaxy, Galaxy)]
pairings gs = [(g1, g2) | g1 <- gs, g2 <- gs, g1 > g2]

shortestPath :: Galaxy -> Galaxy -> Int
shortestPath (y1, x1) (y2, x2) = abs (y1 - y2) + abs (x1 - x2)

solve :: Scaling -> Universe -> Int
solve scaling = sum . fmap (uncurry shortestPath) . pairings . galaxies scaling

part1 :: Universe -> Int
part1 = solve 2

part2 :: Universe -> Int
part2 = solve 1_000_000

main :: IO ()
main = do
    universe <- Universe . lines <$> readFile "input"
    print $ part1 universe
    print $ part2 universe
