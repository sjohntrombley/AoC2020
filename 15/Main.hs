module Main where

import Data.List (foldl')
import Data.Map.Strict (member, (!), empty, insert)

part1 l = print.head $ iterate (\m->(nextNum m):m) l!!(2020-length l)

nextNum (n:ns) = if elem n ns then length (takeWhile (n/=) ns) + 1 else 0

part2 l = print.fst $ foldl' nextNum2 (last l, buildMap l) [length l..29999999]

buildMap l = foldl' (flip (uncurry insert)) empty $ zip (init l) [1..length l-1]
-- \m (n, c)->insert n c m
-- flip (uncurry insert)

nextNum2 (last, prevs) count
    = let prevs' = insert last count prevs
      in if member last prevs then (count-prevs!last, prevs') else (0, prevs')

main = do putStr "Part 1:\n\t"
          part1 $ reverse [2, 0, 6, 12, 1, 3]
          putStr "Part 2:\n\t"
          part2 [2, 0, 6, 12, 1, 3]
