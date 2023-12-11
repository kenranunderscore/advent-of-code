{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use bimap" #-}

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

newtype Area a = Area {value :: Vector (Vector a)}

instance Show (Area Char) where
    show (Area rows) =
        List.intercalate "\n" (V.toList $ fmap V.toList rows)

area :: Parser (Area Char)
area = do
    Area . V.fromList <$> many1 row
  where
    tile =
        char '.'
            <|> char 'S'
            <|> char '|'
            <|> char '-'
            <|> char 'L'
            <|> char 'J'
            <|> char '7'
            <|> char 'F'
    row = V.fromList <$> (many1' tile <* char '\n')

areaHeight :: Area a -> Int
areaHeight = length . (.value)

areaWidth :: Area a -> Int
areaWidth = length . V.head . (.value)

type Pos = (Int, Int)

tileAt :: Area a -> Pos -> a
tileAt (Area area) (y, x) =
    area ! y ! x

start' :: (a -> Bool) -> Area a -> Pos
start' pred area =
    let
        xs = fmap (V.findIndex pred) area.value
        Just y = V.findIndex Maybe.isJust xs
        Just x = xs ! y
    in
        (y, x)

start :: Area Char -> Pos
start = start' (== 'S')

insideBounds :: Area a -> Pos -> Bool
insideBounds area (y, x) =
    y >= 0 && y < h && x >= 0 && x < w
  where
    w = areaWidth area
    h = areaHeight area

connected' ::
    Eq a =>
    a ->
    a ->
    a ->
    a ->
    a ->
    a ->
    a ->
    Area a ->
    Pos ->
    Pos ->
    Bool
connected' s tb lr tl tr bl br area p@(y, x) p2@(y2, x2)
    | not (insideBounds area p2) = False
    | y2 == y && x2 == x - 1 =
        (origin == s || origin == lr || origin == tr || origin == br)
            && (t == lr || t == tl || t == bl)
    | y2 == y && x2 == x + 1 =
        (origin == s || origin == lr || origin == tl || origin == bl)
            && (t == lr || t == tr || t == br)
    | y2 == y - 1 && x2 == x =
        (origin == s || origin == bl || origin == br || origin == tb)
            && (t == tb || t == tl || t == tr)
    | y2 == y + 1 && x2 == x =
        (origin == s || origin == tb || origin == tl || origin == tr)
            && (t == bl || t == br || t == tb)
    | otherwise = False
  where
    t = tileAt area p2
    origin = tileAt area p

connected :: Area Char -> Pos -> Pos -> Bool
connected = connected' 'S' '|' '-' 'F' '7' 'L' 'J'

connectedTiles :: Area Char -> Pos -> Set Pos
connectedTiles area pos@(y, x) =
    tiles
        & filter (connected area pos)
        & Set.fromList
  where
    tiles = [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]

part1 :: Pos -> Area Char -> (Set Pos, Int)
part1 s area =
    go s' mempty s' 1
  where
    s' = Set.singleton s
    go current previous seen n =
        let
            adjacent = mconcat $ Set.toList $ Set.map (connectedTiles area) current
            next = Set.difference adjacent previous
        in
            if next `Set.isSubsetOf` seen then (seen, n - 1) else go next current (Set.union seen next) (n + 1)

expandArea :: Area Char -> Area Char
expandArea (Area rows) =
    Area (V.fromList <$> V.concatMap expandRow rows)
  where
    expandRow r =
        let largerTiles :: Vector [String] = fmap expandTile r
        in V.fromList [concatMap head largerTiles, concatMap last largerTiles]
    expandTile = \case
        '.' -> [". ", "  "]
        'S' -> ["L-", "  "] -- FIXME
        '|' -> ["| ", "| "]
        '-' -> ["--", "  "]
        'L' -> ["L-", "  "]
        'J' -> ["J ", "  "]
        'F' -> ["F-", "| "]
        '7' -> ["7 ", "| "]

reachableNonLoopNeighbors :: Area Char -> Set Pos -> Pos -> Set Pos
reachableNonLoopNeighbors area mainLoop (y, x) =
    [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]
        & filter (insideBounds area)
        & filter (`Set.notMember` mainLoop)
        & Set.fromList

allReachableFields :: Area Char -> Set Pos -> Set Pos -> Set Pos
allReachableFields area mainLoop =
    go
  where
    go visited =
        -- the dumbest way possible -> should only recurse on the newly added
        -- elements
        let next = Set.union visited $ Set.unions $ Set.map (reachableNonLoopNeighbors area mainLoop) visited
        in if length next == length visited then visited else go next

part2 :: Area Char -> Int
part2 area =
    areaWidth area * areaHeight area
        - outside
        - length (fst $ part1 originalStart area)
  where
    expanded = expandArea area
    originalStart = start area
    newStart = (2 * fst originalStart, 2 * snd originalStart)
    mainLoop = traceShowId $ fst $ part1 newStart expanded
    w = areaWidth expanded
    h = areaHeight expanded
    isEmpty c = c == ' ' || c == '.'
    empties = Set.fromList . filter (isEmpty . tileAt expanded)
    top = [(0, x) | x <- [0 .. w - 1]]
    bottom = [(h - 1, x) | x <- [0 .. w - 1]]
    left = [(y, 0) | y <- [0 .. h - 1]]
    right = [(y, w - 1) | y <- [0 .. h - 1]]
    outside =
        [top, bottom, left, right]
            & fmap empties
            & Set.unions
            & allReachableFields expanded mainLoop
            & Set.filter (\(y, x) -> even y && even x)
            & Set.map (\(y, x) -> (y `div` 2, x `div` 2)) -- for debugging
            & traceShowId
            & length

main :: IO ()
main = do
    input <- T.readFile "input"
    let Right x = parseOnly area input
    -- print $ snd $ part1 (start x) x
    print $ expandArea x
    print $ part2 x
