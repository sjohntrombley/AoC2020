module Main where

import Data.Bool(bool)
import Data.List(foldl1', nub, intersect)
import System.IO(IOMode(ReadMode), openFile, hGetContents)

getGroups [] = []
getGroups ls = takeWhile ([]/=) ls:getGroups ((\x->bool (tail x) x ([]==x)) $ dropWhile ([]/=) ls)

part1 = putStrLn.show.sum.map (length.nub.concat) 

part2 = putStrLn.show.sum.map (length.foldl1' intersect)

main = do f <- openFile "input.txt" ReadMode
          c <- hGetContents f
          let groups = getGroups $ lines c
          putStr "Part 1:\n\t"
          part1 groups
          putStr "Part 2:\n\t"
          part2 groups
