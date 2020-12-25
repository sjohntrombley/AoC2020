module Main where

import Control.Monad (ap)
import Data.Bool (bool)
import Data.Foldable (foldl')
import Data.List (delete, (!!))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (fromList, empty, member, insert)
import System.IO (IOMode(ReadMode), openFile, hGetContents)

foldl1' f xs = fromMaybe (errorWithoutStackTrace "foldl1': empty structure")
                         (foldl' mf Nothing xs)
    where
        mf m y = Just (case m of
                        Nothing -> y
                        Just x -> f x y)

part1 l = print.length.(!!6).iterate cycleState $ fromList l

cycleState activeCells
    = let updateCell oldState state c
              = bool (updateInactive oldState state c)
                     (updateActive oldState state c)
                     (member c oldState)
          updateInactive oldState state c@(x, y, z)
              = let activeNeighbors
                        = length $ filter (`member`oldState)
                                 $ delete (x, y, z) 
                                 $ [(x', y', z')
                                       | x' <- [x-1..x+1],
                                         y' <- [y-1..y+1],
                                         z' <- [z-1..z+1]
                                       ]
                in bool state (insert c state) $ activeNeighbors==3
          updateActive oldState state c@(x, y, z)
              = let activeNeighbors
                        = length $ filter (`member`oldState)
                                 $ delete (x, y, z) 
                                 $ [(x', y', z')
                                       | x' <- [x-1..x+1],
                                         y' <- [y-1..y+1],
                                         z' <- [z-1..z+1]
                                       ]
                in bool state (insert c state) $ activeNeighbors==2 || activeNeighbors==3
          ((xl, yl, zl), (xh, yh, zh)) = getBounds activeCells
          toCheck
              = [(x, y, z)
                  | x <- [xl-1..xh+1],
                    y <- [yl-1..yh+1],
                    z <- [zl-1..zh+1]
                  ]
      in foldl' (updateCell activeCells) empty toCheck

getBounds = ap ((,).getLBounds) getUBounds

getLBounds
    = let tripleMin (x0, y0, z0) (x1, y1, z1)
              = (min x0 x1, min y0 y1, min z0 z1)
      in foldl1' tripleMin

getUBounds
    = let tripleMax (x0, y0, z0) (x1, y1, z1)
              = (max x0 x1, max y0 y1, max z0 z1)
      in foldl1' tripleMax

-- Part 2
part2 = print.length.(!!6).iterate cycleState2.fromList.addDim

cycleState2 activeCells
    = let
        updateCell oldState state c
            = bool
                (updateInactive oldState state c)
                (updateActive oldState state c)
                (member c oldState)
        updateInactive oldState state c@(x, y, z, w)
            = let
                activeNeighbors
                    = length.filter (`member`oldState).delete (x, y, z, w) 
                    $ [
                        (x', y', z', w')
                        | x' <- [x-1..x+1]
                        , y' <- [y-1..y+1]
                        , z' <- [z-1..z+1]
                        , w' <- [w-1..w+1]
                        ]
            in bool state (insert c state) (activeNeighbors==3)
        updateActive oldState state c@(x, y, z, w)
            = let
                activeNeighbors
                    = length.filter (`member`oldState).delete (x, y, z, w)
                    $ [
                        (x', y', z', w')
                        | x' <- [x-1..x+1]
                        , y' <- [y-1..y+1]
                        , z' <- [z-1..z+1]
                        , w' <- [w-1..w+1]
                        ]
            in bool state (insert c state) $ activeNeighbors==2 || activeNeighbors==3
        ((xl, yl, zl, wl), (xh, yh, zh, wh)) = getBounds2 activeCells
        toCheck
            = [
                (x, y, z, w)
                | x <- [xl-1..xh+1]
                , y <- [yl-1..yh+1]
                , z <- [zl-1..zh+1]
                , w <- [wl-1..wh+1]
                ]
    in foldl' (updateCell activeCells) empty toCheck

getBounds2 = ap ((,).getLBounds2) getUBounds2

getLBounds2
    = let
        quadMin (x0, y0, z0, w0) (x1, y1, z1, w1)
            = (min x0 x1, min y0 y1, min z0 z1, min w0 w1)
    in foldl1' quadMin

getUBounds2
    = let
        quadMax (x0, y0, z0, w0) (x1, y1, z1, w1)
            = (max x0 x1, max y0 y1, max z0 z1, max w0 w1)
    in foldl1' quadMax

addDim = map (\(x, y, z) -> (x, y, z, 0))

-- Parse input
getStart s
    = let getActive l y = catMaybes $ zipWith (maybeCoord y) l [0..length l-1]
          maybeCoord y c x = bool Nothing (Just (x, y, 0)) $ c=='#'
      in concat $ zipWith (getActive) (lines s) [0..length (lines s)-1]

main = do start <- openFile "input.txt" ReadMode
                >>= hGetContents
                >>= return.getStart
          putStr "Part 1:\n\t"
          part1 start
          putStr "Part 2:\n\t"
          part2 start
