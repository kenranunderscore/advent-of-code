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
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Debug.Trace

newtype Universe = Universe {value :: Vector (Vector Char)}

fromMatrix :: Vector (Vector a) -> [[a]]
fromMatrix = V.toList . fmap V.toList

toMatrix :: [[a]] -> Vector (Vector a)
toMatrix = V.fromList . fmap V.fromList

instance Show Universe where
    show (Universe m) = List.intercalate "\n" $ fromMatrix m

emptyRows :: Vector (Vector Char) -> [Int]
emptyRows u =
    V.toList $
        V.imapMaybe (\y r -> if all (== '.') r then Just y else Nothing) u

emptyColumns :: Vector (Vector Char) -> [Int]
emptyColumns = emptyRows . toMatrix . List.transpose . fromMatrix

type Galaxy = (Int, Int)

galaxies :: Universe -> [Galaxy]
galaxies u =
    V.toList . V.concat . V.toList $
        V.imapMaybe
            ( \y row ->
                let gs =
                        V.imapMaybe
                            ( \x c ->
                                if c == '#'
                                    then Just (translateRow y, translateColumn x)
                                    else Nothing
                            )
                            row
                in if V.null gs then Nothing else Just gs
            )
            u.value
  where
    rs = emptyRows u.value
    cs = emptyColumns u.value
    translateIndex emptyIndices i = i + length (filter (< i) emptyIndices)
    translateRow = translateIndex rs
    translateColumn = translateIndex cs

pairings :: [Galaxy] -> [(Galaxy, Galaxy)]
pairings gs = [(g1, g2) | g1 <- gs, g2 <- gs, g1 > g2]

shortestPath :: Galaxy -> Galaxy -> Int
shortestPath (y1, x1) (y2, x2) = abs (y1 - y2) + abs (x1 - x2)

part1 :: Universe -> Int
part1 = sum . fmap (uncurry shortestPath) . pairings . galaxies

main :: IO ()
main = do
    universe <- Universe . toMatrix . lines <$> readFile "example"
    print $ galaxies universe
    print $ part1 universe
