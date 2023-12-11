{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
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

data Tile = TopBottom | LeftRight | BottomLeft | BottomRight | TopLeft | TopRight | Start | Empty
    deriving stock (Eq)

instance Show Tile where
    show = \case
        TopBottom -> "║"
        LeftRight -> "═"
        Start -> "S"
        Empty -> " "
        TopLeft -> "╔"
        TopRight -> "╗"
        BottomLeft -> "╚"
        BottomRight -> "╝"

newtype Area a = Area {value :: Vector (Vector a)}

instance Show (Area Tile) where
    show (Area rows) =
        List.intercalate "\n" (V.toList $ fmap showRow rows)
      where
        showRow row = concat $ V.toList $ fmap show row

instance Show (Area Char) where
    show (Area rows) =
        List.intercalate "\n" (V.toList $ fmap V.toList rows)

area :: Parser (Area Tile)
area = do
    Area . V.fromList <$> many1 row
  where
    tile =
        ("." >> pure Empty)
            <|> ("S" >> pure Start)
            <|> ("|" >> pure TopBottom)
            <|> ("-" >> pure LeftRight)
            <|> ("L" >> pure BottomLeft)
            <|> ("J" >> pure BottomRight)
            <|> ("7" >> pure TopRight)
            <|> ("F" >> pure TopLeft)
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

start :: Area Tile -> Pos
start = start' (== Start)

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

connected :: Area Tile -> Pos -> Pos -> Bool
connected = connected' Start TopBottom LeftRight TopLeft TopRight BottomLeft BottomRight

connectedTiles :: Area Tile -> Pos -> Set Pos
connectedTiles area pos@(y, x) =
    tiles
        & filter (connected area pos)
        & Set.fromList
  where
    tiles = [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]

part1 :: Area Tile -> (Set Pos, Int)
part1 area =
    go s mempty s 1
  where
    s = Set.singleton $ start area
    go current previous seen n =
        let
            adjacent = mconcat $ Set.toList $ Set.map (connectedTiles area) current
            next = Set.difference adjacent previous
        in
            if next `Set.isSubsetOf` seen then (seen, n - 1) else go next current (Set.union seen next) (n + 1)

main :: IO ()
main = do
    input <- T.readFile "input"
    let Right x = parseOnly area input
    print $ snd $ part1 x
