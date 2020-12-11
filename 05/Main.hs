module Main where

import Control.Monad(ap)
import Data.Bool(bool)
import Numeric(readInt)
import System.IO(IOMode(ReadMode), openFile, hGetContents)

decodeSeat = fst.head.readInt 2 (`elem`"FLBR") (bool 1 0.(`elem`"FL"))

part1 = putStrLn.show.maximum

sort [] = []
sort (x:xs) = sort (filter (x>) xs) ++ x : sort (filter (x<=) xs)

part2 = putStrLn.show.(1+).fst.head.dropWhile (uncurry ((==).(+1))).ap (zip.sort) (tail.sort)

main = do f <- openFile "input.txt" ReadMode
          c <- hGetContents f
          let decodedSeats = map decodeSeat $ lines c
          putStr "Part 1:\n\t"
          part1 decodedSeats
          putStr "Part 2:\n\t"
          part2 decodedSeats
