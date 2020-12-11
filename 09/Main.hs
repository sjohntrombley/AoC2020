module Main where

import Data.Bool (bool)
import System.IO (IOMode(ReadMode), openFile, hGetContents)

sumInLast25 (_:[]) n = False
sumInLast25 (m:l) n = bool (sumInLast25 l n) True $ elem (n-m) l

findNoSum l
    | length l > 25
        = bool (l!!25) (findNoSum $ tail l).sumInLast25 (take 25 l) $ l!!25
    | otherwise = error "All numbers after the preamble are the sum of two of the previous 25 numbers in the list."

part1 = putStrLn.show.findNoSum

findSum n l
    = let sumi' = dropWhile ((n>).sum.flip take l)  [0..length l-1]
          sumi = head sumi'
          answer = minimum (take sumi l) + maximum (take sumi l)
      in case sumi' of
             [] -> error "No solution found."
             otherwise -> bool (findSum n $ tail l)
                               (minimum (take sumi l) + maximum (take sumi l))
                               (sum (take sumi l) == n)
-- \m->sum (take m l) < n
-- (n>).sum.flip take l

part2 = putStrLn.show.findSum 530627549

main = do f <- openFile "input.txt" ReadMode
          c <- hGetContents f
          let ns = map read $ lines c 
          putStr "Part 1:\n\t"
          part1 ns
          putStr "Part 2:\n\t"
          part2 ns

