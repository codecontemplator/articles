-- https://adventofcode.com/2020/day/20
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (Maybe)
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)

type Side = [Bool]

data TileF a = Tile { _tileId :: Int, _tileSides :: a } deriving Functor

type Tile = TileF [Side]

instance Eq (TileF a) where
    t1 == t2 = _tileId t1 == _tileId t2

rotationsAndFlips :: Tile -> [Tile]
rotationsAndFlips t = concatMap rotations [t, flipTile t]
    where
        rotateTile = fmap (\(x:xs) -> xs ++ [x])
        flipTile   = fmap (\[s1,s2,s3,s4] -> map reverse [s3, s2, s1, s4])
        rotations  = take 4 . iterate rotateTile

type Index = (Int,Int)

data Board = Board { _indexToTile :: Map Index Tile, _holes :: [Index] }

emptyBoard :: Board
emptyBoard = Board Map.empty [(0,0)]

addTileToBoard :: Tile -> Index -> Board -> Board
addTileToBoard t i@(x,y) (Board indexToTile holes) = Board indexToTile' holes'
    where
        indexToTile' = Map.insert i t indexToTile
        holes' =
            [ h | h <- holes, h /= i] <>
            [ h | h <- [ (x-1,y), (x+1,y), (x,y+1), (x,y-1) ], Maybe.isNothing (Map.lookup h indexToTile), not (elem h holes) ]

matches :: Tile -> Index -> Board -> Bool
matches (Tile _ [up,right,down,left]) hole (Board indexToTile _) = all match (neighbors hole)
    where
        neighbors (x,y) =
                [
                  (PUp,    (x  , y+1)),
                  (PRight, (x+1, y  )),
                  (PDown,  (x  , y-1)),
                  (PLeft,  (x-1, y  ))
                ]

        match (placement, neighborIndex) =
            case (placement, _tileSides <$> Map.lookup neighborIndex indexToTile) of
                (_,      Nothing                             ) -> True
                (PDown,  Just [nup , _      , _     , _     ]) -> nup    == reverse down
                (PLeft,  Just [_   , nright , _     , _     ]) -> nright == reverse left
                (PUp,    Just [_   , _      , ndown , _     ]) -> ndown  == reverse up
                (PRight, Just [_   , _      , _     , nleft ]) -> nleft  == reverse right

data Placement = PLeft | PRight | PUp | PDown

type Pool = [Tile]

data Seed = Seed {  _pool :: Pool, _board :: Board }

removeTileFromPool :: Tile -> Pool -> Pool
removeTileFromPool t = filter (/=t)

data SearchTreeF a = Leaf Board | NodeF [a] deriving Functor

type Coalgebra f a = a -> f a
type Algebra f a = f a -> a

hylo :: Functor f => Algebra f a -> Coalgebra f b -> b -> a
hylo f g = f . fmap (hylo f g) . g

buildSearchTree :: Coalgebra SearchTreeF Seed
buildSearchTree (Seed [] board) = Leaf board
buildSearchTree (Seed pool board) =
        trace ("pool size = " ++ (show . length $ pool) ++ ", holes = " ++ (show . length . _holes $ board)) $
            NodeF [ Seed (removeTileFromPool t pool) (addTileToBoard t' hole board) |
                        t  <- pool,
                        t' <- rotationsAndFlips t,
                        hole  <- _holes board,
                        matches t' hole board
                ]

getCorners :: Algebra SearchTreeF [[Tile]]
getCorners (NodeF solutions) = concat solutions
getCorners (Leaf board) =
    let
        indexToTile = _indexToTile board
        indices = Map.keysSet indexToTile
        (xCoords, yCoords) = (Set.map fst indices, Set.map snd indices)
        cornerTiles = do
            minX <- Set.lookupMin xCoords
            maxX <- Set.lookupMax xCoords
            minY <- Set.lookupMin yCoords
            maxY <- Set.lookupMax yCoords
            mapM (`Map.lookup` indexToTile) [(minX,minY),(minX,maxY),(maxX,minY),(maxX,maxY)]
    in
        case cornerTiles of
            Just tiles -> [tiles]
            Nothing -> []

solve :: [Tile] -> Int
solve tiles =
    let corners = head $ hylo getCorners buildSearchTree (Seed tiles emptyBoard)
    in foldr ((*) . _tileId) 1 corners


parseTile :: [String] -> Tile
parseTile (header:bitsRaw) =
    let
        id = read $ take 4 $ drop 5 $ header
        bits = map (map (=='#')) bitsRaw
        bitsRot = transpose bits
            where
                transpose ([]:_) = []
                transpose x = map head x : transpose (map tail x)
    in
        Tile id [head bits, last bitsRot, reverse $ last bits, reverse $ head bitsRot]

parseTiles :: String -> [Tile]
parseTiles input =
        let rows = filter (/="") $ lines input
        in map parseTile (group 11 rows)
    where
        group :: Int -> [a] -> [[a]]
        group _ [] = []
        group n l
            | n > 0 = take n l : group n (drop n l)
            | otherwise = error "Negative or zero n"


main :: IO ()
main = do
        input <- readFile "input.txt"
        let tiles = parseTiles input
        let solution = solve tiles
        putStrLn $ "result=" ++ show solution
        if solution == 20899048083289 then do
            putStrLn "sample correct"
        else if solution == 79412832860579 then do
            putStrLn "real correct"
        else do
            putStrLn "---"

