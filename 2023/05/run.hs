{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
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

data SeedRange = SeedRange
    { from :: Integer
    , to :: Integer
    }
    deriving stock (Eq, Generic)

mkSeedRange :: Integer -> Integer -> SeedRange
mkSeedRange from to =
    if from > to
        then error $ "invalid range: " <> show (from, to)
        else SeedRange{from, to}

instance Show SeedRange where
    show s = "[" <> show s.from <> ".." <> show s.to <> "]"

instance Ord SeedRange where
    s <= s' = s.from <= s'.from

instance QC.Arbitrary SeedRange where
    arbitrary = do
        QC.NonNegative from <- QC.arbitrary
        QC.NonNegative offset <- QC.arbitrary
        pure $ SeedRange{from, to = from + offset}

seedCount :: SeedRange -> Integer
seedCount s = s.to - s.from + 1

data Mapping = Mapping
    { dest :: Integer
    , from :: Integer
    , to :: Integer
    }
    deriving stock (Show, Eq, Generic)

instance QC.Arbitrary Mapping where
    arbitrary = do
        QC.NonNegative dest <- QC.arbitrary
        QC.NonNegative from <- QC.arbitrary
        QC.NonNegative offset <- QC.arbitrary
        pure $ Mapping{dest, from, to = from + offset}

mappingFunction :: Mapping -> Integer -> Integer
mappingFunction m i = i + m.dest - m.from

type MappingGroup = [Mapping]

seeds :: Parser [SeedRange]
seeds = do
    string "seeds:"
    many1' seedRange
  where
    seed = char ' ' *> decimal
    seedRange = do
        from <- seed
        len <- seed
        pure $ mkSeedRange from (from + len - 1)

mappingGroup :: Parser MappingGroup
mappingGroup = do
    skipWhile (not . isDigit)
    many1' (range <* char '\n')
  where
    range = do
        dest <- decimal
        from <- char ' ' *> decimal
        len <- char ' ' *> decimal
        pure $ Mapping dest from (from + len - 1)

seedsAndMappings :: Parser ([SeedRange], [MappingGroup])
seedsAndMappings = do
    let group = char '\n' *> mappingGroup
    xs <- seeds
    char '\n'
    groups <- many1' group
    pure (xs, groups)

applySingleMapping :: Mapping -> SeedRange -> Maybe (SeedRange, [SeedRange])
applySingleMapping m s
    | m.from < s.from =
        if
            | m.to < s.from ->
                Nothing
            | m.to < s.to ->
                -- left part of range only
                Just (reallyMap s.from m.to, [mkSeedRange (m.to + 1) s.to])
            | True ->
                -- whole range is mapped
                Just (reallyMap s.from s.to, mempty)
    | m.from == s.from =
        if
            | m.to < s.to ->
                Just (reallyMap m.from m.to, [mkSeedRange (m.to + 1) s.to])
            | True ->
                -- whole range is mapped
                Just (reallyMap s.from s.to, mempty)
    | m.from < s.to =
        if
            | m.to < s.to ->
                -- mapping fully contained -> two unmapped ranges
                Just (reallyMap m.from m.to, [mkSeedRange s.from (m.from - 1), mkSeedRange (m.to + 1) s.to])
            | True ->
                Just (reallyMap m.from s.to, [mkSeedRange s.from (m.from - 1)])
    | m.from == s.to =
        Just (reallyMap s.to s.to, [mkSeedRange s.from (s.to - 1)])
    | m.from > s.to =
        Nothing
    | True =
        error "impossible m.from"
  where
    reallyMap from to = mkSeedRange (mappingFunction m from) (mappingFunction m to)

applyGroup :: MappingGroup -> SeedRange -> [SeedRange]
applyGroup mg s =
    go mg [] [s]
  where
    go _ acc [] = acc
    go group acc rs@(r : remainingRanges) =
        case group of
            [] -> rs <> acc
            (m : rest) ->
                case applySingleMapping m r of
                    Nothing -> go rest acc rs
                    Just (mapped, unmapped) ->
                        go rest (mapped : acc) (remainingRanges <> unmapped)

applyGroups :: [MappingGroup] -> [SeedRange] -> [SeedRange]
applyGroups groups ranges =
    foldl'
        (\prev mg -> concatMap (applyGroup mg) prev)
        ranges
        groups

part1 :: Text -> Integer
part1 input = do
    let
        Right (seedRanges, groups) = parseOnly seedsAndMappings input
        singleSeeds = concatMap (\s -> [mkSeedRange s.from s.from, mkSeedRange (seedCount s) (seedCount s)]) seedRanges
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

prop_applySingleMappingKeepsNumberOfSeedsFixed :: (Mapping, SeedRange) -> Bool
prop_applySingleMappingKeepsNumberOfSeedsFixed (m, s) =
    let res = applySingleMapping m s
    in case res of
        Nothing -> True
        Just (mapped, unmapped) ->
            seedCount mapped + sum (fmap seedCount unmapped) == seedCount s

prop_applyGroupsKeepsNumberOfSeedsFixed :: ([MappingGroup], [SeedRange]) -> Bool
prop_applyGroupsKeepsNumberOfSeedsFixed (groups, ranges) =
    let res = applyGroups groups ranges
    in sum (fmap seedCount res) == sum (fmap seedCount ranges)

prop_applyGroupKeepsNumberOfSeedsFixed :: (MappingGroup, SeedRange) -> Bool
prop_applyGroupKeepsNumberOfSeedsFixed (mg, s) =
    let res = applyGroup mg s
    in sum (fmap seedCount res) == seedCount s
