module Main where

import Control.Monad (ap)
import Data.List (foldl')
import System.Environment (getArgs)
import System.IO (IOMode(ReadMode), openFile, hGetContents)

part1 = print.uncurry ((.abs).(+).abs).fst.foldl' moveBoat ((0, 0), (1, 0)) 

moveBoat ((x, y), (dx, dy)) (c, n)
    = case c of
          'N' -> ((x, y+n), (dx, dy))
          'E' -> ((x+n, y), (dx, dy))
          'S' -> ((x, y-n), (dx, dy))
          'W' -> ((x-n, y), (dx, dy))
          'F' -> ((x+n*dx, y+n*dy), (dx, dy))
          'R' -> case n of 90 -> ((x, y), (dy, -dx))
                           180 -> ((x, y), (-dx, -dy))
                           270 -> ((x, y), (-dy, dx))
          'L' -> case n of 90 -> ((x, y), (-dy, dx))
                           180 -> ((x, y), (-dx, -dy))
                           270 -> ((x, y), (dy, -dx))

part2 = print.uncurry ((.abs).(+).abs).fst.foldl' moveBoat2 ((0, 0), (10, 1)) 

moveBoat2 ((x, y), (wx, wy)) (c, n)
    = case c of
          'N' -> ((x, y), (wx, wy+n))
          'E' -> ((x, y), (wx+n, wy))
          'S' -> ((x, y), (wx, wy-n))
          'W' -> ((x, y), (wx-n, wy))
          'F' -> ((x+n*wx, y+n*wy), (wx, wy))
          'R' -> case n of 90 -> ((x, y), (wy, -wx))
                           180 -> ((x, y), (-wx, -wy))
                           270 -> ((x, y), (-wy, wx))
          'L' -> case n of 90 -> ((x, y), (-wy, wx))
                           180 -> ((x, y), (-wx, -wy))
                           270 -> ((x, y), (wy, -wx))

main = do fn <- getArgs >>= return.head
          f <- openFile fn ReadMode
          c <- hGetContents f
          let insts = map (ap ((,).head) (read.tail)) $ lines c
          putStr "Part 1:\n\t"
          part1 insts
          putStr "Part 2:\n\t"
          part2 insts
