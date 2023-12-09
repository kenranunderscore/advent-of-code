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
    deriving stock (Show, Eq, Generic)

instance QC.Arbitrary SeedRange where
    arbitrary = SeedRange <$> QC.arbitrary <*> QC.arbitrary

seeds :: Parser [SeedRange]
seeds = do
    string "seeds:"
    many1' seedRange
  where
    seed = char ' ' *> decimal
    seedRange = do
        from <- seed
        len <- seed
        pure $ SeedRange from (from + len - 1)

data Mapping = Mapping
    { dest :: Integer
    , from :: Integer
    , to :: Integer
    }
    deriving stock (Show, Eq, Generic)

instance QC.Arbitrary Mapping where
    arbitrary = Mapping <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary

mappingFunction :: Mapping -> Integer -> Integer
mappingFunction m i = i + m.dest - m.from

type MappingGroup = [Mapping]

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

applyGroups :: [MappingGroup] -> SeedRange -> [SeedRange]
applyGroups groups s =
    foldl'
        ( \prev mg ->
            let x = concatMap (applyGroup mg) prev
            in -- trace ("    after group: " <> show x) x
               x
        )
        [s]
        groups

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

applySingleMapping :: Mapping -> SeedRange -> Maybe (SeedRange, [SeedRange])
applySingleMapping m s =
    if
        | m.to < s.from || m.from > s.to ->
            Nothing
        | m.from <= s.from && m.to >= s.to ->
            -- all seeds are mapped
            Just (reallyMap s.from s.to, mempty)
        | m.from <= s.from && m.to < s.to ->
            -- overlap to the left
            Just
                ( reallyMap s.from m.to
                , [SeedRange (m.to + 1) s.to]
                )
        | m.from <= s.to && m.to >= s.to ->
            -- overlap to the right
            Just
                ( reallyMap m.from s.to
                , [SeedRange s.from (m.from - 1)]
                )
        | m.from > s.from && m.to < s.to ->
            -- mapping an inner part only
            Just
                ( reallyMap m.from m.to
                , [SeedRange s.from (m.from - 1), SeedRange (m.to + 1) s.to]
                )
        | True -> error "forgotten case?"
  where
    reallyMap from to = SeedRange (mappingFunction m from) (mappingFunction m to)

seedCount :: SeedRange -> Integer
seedCount s = s.to - s.from + 1

prop_applySingleMappingKeepNumberOfSeedsFixed :: (Mapping, SeedRange) -> Bool
prop_applySingleMappingKeepNumberOfSeedsFixed (m, s) =
    let res = applySingleMapping m s
    in case res of
        Nothing -> True
        Just (mapped, unmapped) ->
            seedCount mapped + sum (fmap seedCount unmapped) == seedCount s

part1 :: Text -> Integer
part1 input = do
    let
        Right (seedRanges, groups) = parseOnly seedsAndMappings input
        singleSeeds = concatMap (\s -> [SeedRange s.from s.from, SeedRange s.to s.to]) seedRanges
        res = traceShowId $! concatMap (applyGroups groups) singleSeeds
    minimum $ fmap (.from) res

part2 :: Text -> Integer
part2 input = do
    let
        Right (seedRanges, groups) = parseOnly seedsAndMappings input
        res = traceShowId $! concatMap (applyGroups groups) seedRanges
    minimum $ fmap (.from) res

main :: IO ()
main = do
    input <- T.readFile "input"
    print $ part1 input
    print $ part2 input
