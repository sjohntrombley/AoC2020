module Main where

import qualified Text.Regex as R
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Data.List(foldl')
import Data.Maybe(isJust)

data Side = T | R | B | L
data Flip = None | Horizontal | Vertical

type Edge = [Bool]
type Tile = (Int, Edge, Edge, Edge, Edge)
type TileMap = M.Map Int Tile
type EdgeMap = M.Map Edge (Int, Side, Bool)
type Grid = [[Int]]

data Puzzle = Puzzle {tmap :: TileMap, emap :: EdgeMap}
data Edges = Edges {
    topEdge :: [Edge],
    rightEdge :: [Edge],
    bottomEdge :: [Edge],
    leftEdge :: [Edge] }
data SolveState = SS {grid :: Grid,  edges :: Edges, used :: S.Set Int}

foldl _ z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

foldr _ z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

strToMEdge :: String -> Maybe Edge
strToMEdge =
    let
        f c m
            | c elem "#."   = m >>= \e -> Just $ (c=='#'):e
            | otherwise     = Nothing
    in foldr f $ Just []

-- Returns the tile number followed by the top, right, bottom, and left edges
loadTile :: String -> Maybe (Int, Edge, Edge, Edge, Edge)
loadTile s =
    let
        ls = lines s
        tile = tail ls
        tileRegex = R.mkRegex "^Tile (0|[1-9][0-9]*):$"
        mTileNum =
            R.matchRegex tileRegex (head ls) >>= \tn -> Just . read $ head tn
        mTopE = strToMEdge $ head tile
        mRightE = strToMEdge $ map last tile
        mBottomE = strToMEdge.reverse $ last tile
        mLeftE = strToMEdge.reverse $ map head tile
    in do
        tn      <- mTileNum
        topE    <- mTopE
        rightE  <- mRightE
        bottomE <- mBottomE
        leftE   <- mLeftE
        return (tn, topE, rightE, bottomE, leftE)

addTile :: Puzzle -> Tile -> Puzzle
addTile (tileMap, edgeMap) (tn, topE, rightE, bottomE, leftE) =
    let
        newTileMap = M.insert tn (topE, rightE, bottomE, leftE) tileMap
        newEdgeMap = foldl'
            (\m (e, s, f)->M.insertWith (flip (++)) e [(tn, s, f)] m)
            edgeMap
            [   (topE,              T,  False),
                (rightE,            R,  False),
                (bottomE,           B,  False),
                (leftE,             L,  False),
                (reverse topE,      T,  True),
                (reverse rightE,    R,  True),
                (reverse bottomE,   B,  True),
                (reverse leftE,     L,  True) ]
    in (newTileMap, newEdgeMap)

--solve :: Puzzle -> SolveState -> Maybe Integer

firstCol :: Puzzle -> SolveState -> Maybe Integer
firstCol 

main :: IO ()
main = putStrLn "Hello, Haskell!"
