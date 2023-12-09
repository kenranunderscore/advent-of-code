{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

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

data Range = Range
    { from :: Integer
    , to :: Integer
    }
    deriving stock (Eq, Generic)

mkRange :: Integer -> Integer -> Range
mkRange from to =
    if from > to
        then error $ "invalid range: " <> show (from, to)
        else Range{from, to}

instance Show Range where
    show s = " [" <> show s.from <> " .. " <> show s.to <> "] "

instance Ord Range where
    s <= s' = s.from <= s'.from

instance QC.Arbitrary Range where
    arbitrary = do
        QC.NonNegative from <- QC.arbitrary
        QC.NonNegative offset <- QC.arbitrary
        pure $ Range{from, to = from + offset}

rangeLength :: Range -> Integer
rangeLength s = s.to - s.from + 1

data MappingRow = MappingRow
    { dest :: Integer
    , from :: Integer
    , to :: Integer
    }
    deriving stock (Show, Eq, Generic)

instance QC.Arbitrary MappingRow where
    arbitrary = do
        QC.NonNegative dest <- QC.arbitrary
        QC.NonNegative from <- QC.arbitrary
        QC.NonNegative offset <- QC.arbitrary
        pure $ MappingRow{dest, from, to = from + offset}

mappingFunction :: MappingRow -> Integer -> Integer
mappingFunction m i = i + m.dest - m.from

type Mapping = [MappingRow]

seeds :: Parser [Range]
seeds = "seeds:" *> many1' seedRange
  where
    seed = char ' ' *> decimal
    seedRange = do
        from <- seed
        len <- seed
        pure $ mkRange from (from + len - 1)

mapping :: Parser Mapping
mapping = do
    skipWhile (not . isDigit)
    many1' (range <* char '\n')
  where
    range = do
        dest <- decimal
        from <- char ' ' *> decimal
        len <- char ' ' *> decimal
        pure $ MappingRow dest from (from + len - 1)

seedsAndMappings :: Parser ([Range], [Mapping])
seedsAndMappings = do
    let group = char '\n' *> mapping
    xs <- seeds
    char '\n'
    groups <- many1' group
    pure (xs, groups)

applyMappingRow :: MappingRow -> Range -> Maybe (Range, [Range])
applyMappingRow m s
    | m.from < s.from =
        if
            | m.to < s.from ->
                Nothing
            | m.to < s.to ->
                -- left part of range only
                Just (reallyMap s.from m.to, [mkRange (m.to + 1) s.to])
            | True ->
                -- whole range is mapped
                Just (reallyMap s.from s.to, mempty)
    | m.from == s.from =
        if
            | m.to < s.to ->
                Just (reallyMap m.from m.to, [mkRange (m.to + 1) s.to])
            | True ->
                -- whole range is mapped
                Just (reallyMap s.from s.to, mempty)
    | m.from < s.to =
        if
            | m.to < s.to ->
                -- mapping fully contained -> two unmapped ranges
                Just (reallyMap m.from m.to, [mkRange s.from (m.from - 1), mkRange (m.to + 1) s.to])
            | True ->
                Just (reallyMap m.from s.to, [mkRange s.from (m.from - 1)])
    | m.from == s.to =
        Just (reallyMap s.to s.to, [mkRange s.from (s.to - 1)])
    | m.from > s.to =
        Nothing
    | otherwise =
        error "impossible m.from"
  where
    reallyMap from to = mkRange (mappingFunction m from) (mappingFunction m to)

applyGroup :: Mapping -> Range -> [Range]
applyGroup mg s =
    go mg [] [s]
  where
    go _ acc [] = acc
    go group acc rs@(r : remainingRanges) =
        case group of
            [] -> rs <> acc
            (m : rest) ->
                case applyMappingRow m r of
                    Nothing -> go rest acc rs
                    Just (mapped, unmapped) ->
                        go rest (mapped : acc) (remainingRanges <> unmapped)

applyGroups :: [Mapping] -> [Range] -> [Range]
applyGroups groups ranges =
    foldl'
        (\prev mg -> concatMap (applyGroup mg) prev)
        ranges
        groups

part1 :: Text -> Integer
part1 input = do
    let
        Right (seedRanges, groups) = parseOnly seedsAndMappings input
        singleSeeds = concatMap (\s -> [mkRange s.from s.from, mkRange (rangeLength s) (rangeLength s)]) seedRanges
        res = applyGroups groups singleSeeds
    minimum $ fmap (.from) res

part2 :: Text -> Integer
part2 input = do
    let
        Right (seedRanges, groups) = parseOnly seedsAndMappings input
        res = traceShowId $! sort $ applyGroups groups seedRanges
    minimum $ fmap (.from) res

main :: IO ()
main = do
    input <- T.readFile "input"
    print $ part1 input
    print $ part2 input

prop_applyMappingRowKeepsNumberOfSeedsFixed :: (MappingRow, Range) -> Bool
prop_applyMappingRowKeepsNumberOfSeedsFixed (m, s) =
    let res = applyMappingRow m s
    in case res of
        Nothing -> True
        Just (mapped, unmapped) ->
            rangeLength mapped + sum (fmap rangeLength unmapped) == rangeLength s

prop_applyGroupsKeepsNumberOfSeedsFixed :: ([Mapping], [Range]) -> Bool
prop_applyGroupsKeepsNumberOfSeedsFixed (groups, ranges) =
    let res = applyGroups groups ranges
    in sum (fmap rangeLength res) == sum (fmap rangeLength ranges)

prop_applyGroupKeepsNumberOfSeedsFixed :: (Mapping, Range) -> Bool
prop_applyGroupKeepsNumberOfSeedsFixed (mg, s) =
    let res = applyGroup mg s
    in sum (fmap rangeLength res) == rangeLength s
