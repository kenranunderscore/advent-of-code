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

newtype Area = Area {value :: Vector (Vector Tile)}

instance Show Area where
    show (Area rows) =
        List.intercalate "\n" (V.toList $ fmap showRow rows)
      where
        showRow row = concat $ V.toList $ fmap show row

area :: Parser Area
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

areaHeight :: Area -> Int
areaHeight = length . (.value)

areaWidth :: Area -> Int
areaWidth = length . V.head . (.value)

type Pos = (Int, Int)

tileAt :: Area -> Pos -> Tile
tileAt (Area area) (y, x) =
    area ! y ! x

start :: Area -> Pos
start (Area area) =
    let
        xs = fmap (V.findIndex (== Start)) area
        Just y = V.findIndex Maybe.isJust xs
        Just x = xs ! y
    in
        (y, x)

connectedTiles :: Area -> Pos -> Set Pos
connectedTiles area pos@(y, x) =
    let
        origin = tileAt area pos
        tiles =
            [ if y - 1 >= 0
                && let t = tileAt area (y - 1, x)
                   in (origin == Start || origin == BottomLeft || origin == BottomRight || origin == TopBottom)
                        && (t == TopBottom || t == TopLeft || t == TopRight)
                then Just (y - 1, x)
                else Nothing
            , if y + 1 < areaHeight area
                && let t = tileAt area (y + 1, x)
                   in (origin == Start || origin == TopBottom || origin == TopLeft || origin == TopRight)
                        && (t == BottomLeft || t == BottomRight || t == TopBottom)
                then Just (y + 1, x)
                else Nothing
            , if x - 1 >= 0
                && let t = tileAt area (y, x - 1)
                   in (origin == Start || origin == LeftRight || origin == TopRight || origin == BottomRight)
                        && (t == LeftRight || t == TopLeft || t == BottomLeft)
                then Just (y, x - 1)
                else Nothing
            , if x + 1 < areaWidth area
                && let t = tileAt area (y, x + 1)
                   in (origin == Start || origin == LeftRight || origin == TopLeft || origin == BottomLeft)
                        && (t == LeftRight || t == TopRight || t == BottomRight)
                then Just (y, x + 1)
                else Nothing
            ]
    in
        Set.fromList $ Maybe.catMaybes tiles

part1 :: Area -> Int
part1 area =
    1 + go (Set.singleton (start area)) mempty 0
  where
    go current previous n =
        let
            adjacent = mconcat $ Set.toList $ Set.map (connectedTiles area) current
            next = Set.difference adjacent previous
        in
            if length next == 1 then n else go next current (n + 1)

main :: IO ()
main = do
    input <- T.readFile "input"
    let Right x = parseOnly area input
    print $ part1 x
