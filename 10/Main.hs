module Main where

import Data.List (sort)
import System.IO (IOMode(ReadMode), openFile, hGetContents)

differences l = let l' = 0:sort l++[maximum l+3]
                    diffs = zipWith (-) (tail l') l'
                    count n = length $ filter (n==) diffs
                in count 1 * count 3

part1 = putStrLn.show.differences

combinations s [] l | length l < 3 = s+1
combinations s (n:ns) l | length l < 3 = combinations (s+1) ns n
combinations s ns (a:b:c:l)
    | c-a<4 = combinations s ((a:c:l):ns) (b:c:l)
    | otherwise = combinations s ns (b:c:l)

splitAdapters [] = [[]]
splitAdapters l  
    | length l > 2
        = let firstDropable
                  = dropWhile (\n->(l!!(n+1))-(l!!(n-1))>3)
                              [1..length l - 2]
              fd = head firstDropable
              firstAfterDropable
                  = dropWhile (\n->(l!!(n+1))-(l!!(n-1))<4)
                              [fd..length l - 2]
              fad = head firstAfterDropable
          in case (firstDropable, firstAfterDropable) of
              ([], _) -> [[]]
              (_, []) -> [drop (fd-1) l]
              (_, _) -> drop (fd-1) (take (fad+1) l):splitAdapters (drop fad l)
    | otherwise = [[]]

part2 l = putStrLn.show.product.map (combinations 0 []).splitAdapters $ 0:sort l++[maximum l+3]

main :: IO ()
main = do f <- openFile "input.txt" ReadMode
          c <- hGetContents f>>=return.map read.lines
          putStr "Part 1:\n\t"
          part1 c
          putStr "Part 2:\n\t"
          part2 c
