module Main where

import Prelude hiding (Either(..))

import Data.Bool (bool)
import Data.Foldable (foldl')
import Data.List.Split (splitOn)
import Data.Map.Strict (Map, empty, keys, (!))
import qualified Data.Map.Strict as Map (insert)
import Data.Set (Set, member, notMember)
import qualified Data.Set as Set (singleton, insert)
import Data.Sequence (Seq(..), (><))
import qualified Data.Sequence as Seq (reverse, singleton, splitAt)
import System.Environment (getArgs)
import System.IO (IOMode(ReadMode), openFile, hGetContents)

data Side = Top | Right | Bottom | Left
type EdgeMap = Map String (Set (Int, Side, Bool))
type TileMap = Map Int (String, String, String, String)
data ImageState
    = IS
        { edgeMap :: EdgeMap
        , tileMap :: TileMap
        , used :: Set Int
        , sideLength :: Int
        , edges :: (Seq String, Seq String, Seq String, Seq String)
        }

-- Input parsing
parseInput :: String -> (EdgeMap, TileMap)
parseInput s
    = let
        tileStrings = splitOn "\n\n" s
        addTile _ []
            = error "parseInput: You have 4 newlines in a row somehow"
        addTile (em, tm) (s:ss)
            = let
                i = bool
                    (error "parseInput: tile lacks correct header")
                    (bool
                        (error "parseInput: tile number isn't a number")
                        (read . init $ drop 5 s)
                        (all isNumber . init $ drop 5 s))
                    ("Tile " == take 5 s)
                tTop = head ss
                tRight = map last ss
                tBottom = last ss
                tleft = map head ss
                em'
                    =let mkv k t f = (k, [(i, t, f)])
                    in foldl' (flip.uncurry $ insertWith (++)) em
                        [ mkv tTop Top False
                        , mkv tRight Right False
                        , mkv tBottom Bottom False
                        , mkv tLeft Left False
                        , mkv (reverse tTop) Top True
                        , mkv (reverse tRight) Right True
                        , mkv (reverse tBottom) Bottom True
                        , mkv (reverse tLeft) Left True ]
            in (em', Map.insert i (tTop, tRight, tBottom, tLeft) tm)
    in foldl' addTitle (empty, empty) tileStrings

-- First we get a list of possible columns from a seed tile
getPossibleCols :: EdgeMap -> TileMap -> Int -> [(ImageState, [Int])]
getPossibleCols edgeMap tileMap tile
    = let
        sideLength = floor.sqrt.length $ tileMap
        (top, right, bottom, left) = tileMap tile
        s = IS
            edgeMap
            tileMap
            (Set.singleton tile)
            sideLength
            ( Seq.singleton top
            , Seq.singleton right
            , Seq.singleton bottom
            , Seq.singleton left )
        addUp n = foldMap
            (\p->expandUp (flipCol p) $ sideLength-n-1)
            (expandUp (s, [tile]) n)
    in foldMap addUp [0..sideLength-1]

flipCol :: (ImageState, [Int]) -> (ImageState, [Int])
flipCol (s, c)
    = let (topEdges, rightEdges, bottomEdges, leftEdges) = edges s
    in 
        ( s { edges = 
            ( bottomEdges
            , Seq.reverse . fmap reverse $ rightEdges
            , topEdges
            , Seq.reverse . fmap reverse $ leftEdges )}
        , reverse c)

expandUp :: (ImageState, [Int]) -> Int -> [(ImageState, [Int])]
expandUp (s, c) 0 = [(s, c)]
expandUp (s, c) n
    = let
        (tops, rights, bottoms, lefts) = edges s
        edge = case tops of
            Empty -> error "expandUp: Top edge cannot be empty."
            (top :<| Empty) -> top
            otherwise -> error "expandUp: Top edge too wide."
        posses = foldMap (\e@(i, _, _)->bool [] [e].notMember i $ used s) (edgeMap ! edge)
        addOnePoss (i, side, flipped)
            = let
                (topE, rightE, bottomE, leftE) = tileMap s $ i
                updatePair t r l =
                    ( s
                        { used = insert i $ used s
                        , edges =
                            ( Seq.singleton t
                            , r <| rights
                            , bottoms
                            , l <| lefts ) }
                    , i:c )
            in case (side, flipped) of
                -- 90 cw: (top, right, bottom, left) -> (reverse left, top, reverse right, bottom)
                -- 90 ccw: (top, right, bottom, left) -> (right, reverse bottom, left, referse top)
                -- hflip: (top, right, bottom, left) -> (reverse top, left, reverse bottom, right)
                -- vflip: (top, right, bottom, left) -> (bottom, reverse right, top, reverse left)
                
                -- vflip
                (Top, False) -> expandUp
                    (updatePair bottomE (reverse rightE) $ reverse leftE)
                    (n-1)
                -- 90 cw then hflip
                (Right, False)
                    -> expandUp (updatePair leftE bottomE topE) (n - 1)
                -- Nothing
                (Bottom, False)
                    -> expandUp (updatePair topE rightE leftE) (n - 1)
                -- 90 ccw
                (Left, False) -> expandUp
                    (updatePair rightE (reverse bottomE) $ reverse topE)
                    (n - 1)
                -- vflip then hflip
                (Top, True) -> expandUp
                    (updatePair
                        (reverse bottomE)
                        (reverse leftE)
                        (reverse rightE))
                    (n - 1)
                -- 90 cw
                (Right, True) -> expandUp
                    (updatePair (reverse leftE) topE bottomE)
                    (n - 1)
                -- hflip
                (Bottom, True) -> expandUp
                    (updatePair (reverse topE) leftE rightE)
                    (n - 1)
                -- 90 ccw then hflip 
                (Left, True) -> expandUp
                    (updatePair
                        (reverse rightE)
                        (reverse topE)
                        (reverse bottomE))
                    (n - 1)
    in foldMap addOnePoss posses

-- Second we expand the columns into squares
expandCols :: [(ImageState, [Int])] -> [(ImageState, [[Int]])]
expandCols = foldMap expandCol

-- Takes a column generated in the first bit and returns a list of square grids
-- that contain this column. Should return either the empty list or a
-- singleton.
expandCol :: (ImageState, [Int]) -> [(ImageState, [[Int]])]
expandCol (s, c) 
    = let 
        addLThenR n = foldMap
            (expandColLeft (sideLength s-n-1).flipRect)
            (expandColLeft n (s, [c]))
    in foldMap addLThenR [0..sideLength s - 1]

-- Return a list of possible grids constructed by adding n columns on the left
-- side of the supplied grid.
expandColLeft :: (ImageState, [[Int]]) -> n -> [(ImageState, [[Int]])]
expandColLeft 0 p = [p]
expandColLeft n p
    = foldMap (expandColLeft $ n-1) . foldMap finishLeftCol $ addOneLeft p

-- Takes a grid and flips it horizontally so we can reuse the left expansion
-- code for right expansion.
flipRect :: (ImageState, [[Int]]) -> (ImageState, [[Int]])
flipRect p = undefined

-- Takes a rectangle and returns a list of pseudo-rectangles constructed by
-- adding a single tile to the left hand side on the bottom.
addOneLeft :: (ImageState, [[Int]]) -> [(ImageState, [[Int]])]
addOneLeft (s, g)
    = let
        (topEdges, rightEdges, bottomEdges, leftEdges) = edges s
        (leftEdges', blEdge) = case leftEdges of
            Empty -> error "addOneLeft: leftEdges empty."
            (es:|>e) -> (es, e)
        posses = foldMap
            (\e@(i, _, _) -> bool [e] [] $ member i $ used s)
            (edgeMap s ! blEdge)
        -- Takes tile and adds it to the rectangle provided as an argument to
        -- addOneLeft, returning a pseudo-rectangle. A pseudo-rectangle is
        -- an (ImageState, [[Int]]) pair where the top edge may be discontinuous after the
        -- first element and the left edge may be discontinuous anywhere. This
        -- corresponds to a grid (the second element of the pair) of the form
        -- c:cs where cs is a rectangle and length c <= sideLength.
        addTile (i, side, flipped)
            = let
                (tTop, tRight, tBottom, tLeft) = tileMap s ! i
                addT t b l =
                    ( s
                        { used = Set.insert i $ used s
                        , edges =
                            ( t :<| topEdges
                            , rightEdges
                            , b :<| bottomEdges
                            , leftEdges' :|> l ) }
                    , [i]:g )
            in case (side, flipped) of
                (Top, False) -> addT (reverse tLeft) (reverse tRight) tBottom
                (Right, False) -> addT tTop tBottom tLeft
                (Bottom, False) -> addT tLeft tRight tTop
                (Left, False) -> addT (reverse tTop) (reverse tBottom) tRight
                (Top, True)
                    -> addT (reverse tRight) (reverse tLeft) $ reverse tBottom
                (Right, True) -> addT tBottom tTop $ reverse tLeft
                (Bottom, True) -> addT tRight tLeft $ reverse tTop
                (Left, True)
                    -> addT (reverse tBottom) (reverse tTop) $ reverse tRight
    in map addTile posses

-- Takes a pseudo-rectangle and returns a list of possible rectangles by
-- finding all possible ways to complete the left column.
finishLeftCol :: (ImageState, [[Int]]) -> [(ImageState, [[Int]])]
finishLeftCol (_, []) = error "finishLeftCol: the grid is empty."
finishLeftCol p@(s, c:cs)
    | length c > sideLength s = error "finishLeftCol: left column is too big"
    | length c == sideLength s = [p]
    | otherwise
        = let 
            (topEdges, rightEdges, bottomEdges, leftEdges) = edges s
            -- Strip
            (topMatch, topEdges') = case topEdges of
                Empty -> error "finishLeftCol: top edge is empty"
                (e:<|es) -> (e, es)
            (lbEdges, leftMatch, laEdges)
                = case Seq.splitAt (sideLength s-length c-1) leftEdges of
                    (_, Empty) -> error "finishLeftCol: leftEdge is too short"
                    (lbe, lm :<| lae) -> (lbe, lm, lae)
            botPosses
                = filter (\(i, _, _) -> member i $ used s) $ edgeMap ! topMatch
            fits (i, side, flipped)
                = let 
                    (tTop, tRight, tBottom, tLeft) = tileMap s ! i
                in case (side, flipped) of
                    (Top, False) -> reverse tRight == leftMatch
                    (Right, False) -> tBottom == leftMatch
                    (Bottom, False) -> tRight == leftMatch
                    (Left, False) -> reverse tBottom == leftMatch
                    (Top, True) -> reverse tLeft == leftMatch
                    (Right, True) -> tTop == leftMatch
                    (Bottom, True) -> tLeft == leftMatch
                    (Left, True) -> reverse tTop == leftMatch
            addTile (i, side, flipped)
                = let
                    (tTop, tRight, tBottom, tLeft) = tileMap s ! i
                    update t l =
                        ( s
                            { used = Set.insert i $ used s
                            , edges =
                                ( t :<| topEdges'
                                , rightEdges
                                , bottomEdges
                                , lbEdges >< l :<| laEdges ) }
                            , (i:c):cs )
                in case (side, flipped) of
                    (Top, False) -> update tBottom $ reverse tLeft
                    (Right, False) -> update tLeft tTop
                    (Bottom, False) -> update tTop tLeft
                    (Left, False) -> update tRight $ reverse tTop
                    (Top, True) -> update (reverse tBottom) $ reverse tRight
                    (Right, True) -> update (reverse tLeft) tBottom
                    (Bottom, True) -> update (reverse tTop) tRight
                    (Left, True) -> update (reverse tRight) $ reverse tBottom
            in foldMap (finishLeftCol.addTile) $ filter fits botPosses

main :: IO ()
main = do
    args <- getArgs
    let fname = case args of
            [] -> error "Must call with a file name."
            (fn:_) -> fn
    (em, tm) <- hGetContents (openFile fname ReadMode) >>= return.parseInput
    let tile = case keys tm of
            [] -> error "tile map empty after parse"
            (t:_) -> t
    case expandCols $ getPossibleCols em tm tile of
        [] -> error "failed to find a solution."
        ((_,g):_)
            -> putStr "Part 1:\n\t"
            >> print.product $ map toInteger
                [head (head g), last (head g), head (last g), last (last g)]
