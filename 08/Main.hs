module Main where

import Data.Bool (bool)
import Data.Maybe (fromJust)
import System.IO (IOMode(ReadMode), openFile, hGetContents)
import Text.Regex (mkRegex, matchRegex)

accAtFirstRepeat :: (Num a, Ord a) => [Int] -> a -> [(String, a)] -> [(String, a)]-> (Bool, a)
accAtFirstRepeat _ acc _ [] = (True, acc)
accAtFirstRepeat visited acc prevs ((i, n):nexts)
    | elem (length prevs) visited = (False, acc)
    | i=="acc"
        = accAtFirstRepeat (length prevs:visited) (acc+n) ((i, n):prevs) nexts
    | i=="nop"
        = accAtFirstRepeat (length prevs:visited) acc ((i, n):prevs) nexts
    | i=="jmp"
        = let jmp 0 prev next = (prev, next)
              jmp o (p:prev) (n:next) | o<0 = jmp (o+1) prev (p:n:next)
                                      | o>0 = jmp (o-1) (n:p:prev) next
              (prevs', nexts') = jmp n prevs ((i, n):nexts)
          in accAtFirstRepeat (length prevs:visited) acc prevs' nexts'
    | otherwise
        = error $ "\""++i++"\" is not a valid instruction."

parseLine :: (Num a, Read a) => String -> (String, a)
parseLine s
    = let re = mkRegex "^(acc|jmp|nop) ((\\+|-)(0|[1-9][0-9]*))$"
          res = fromJust $ matchRegex re s
      in bool (res!!0, read $ res!!1) (res!!0, read $ res!!3) (res!!2=="+")

part1 = putStrLn.show.snd.accAtFirstRepeat [] 0 []

flipInstruction program n
    = let prev = take n program
          ((i, a):next) = drop n program
      in case i of "jmp" -> prev ++ ("nop", a):next
                   "nop" -> prev ++ ("jmp", a):next
                   otherwise -> error $ "Can't flip "++i++"."

part2 program
    = let indicesToFlip = filter (("acc"/=).fst.(program!!))
                                 [0..length program-1]
          progResults = map (accAtFirstRepeat [] 0 [].flipInstruction program)
                            indicesToFlip
      in putStrLn.show.snd.head $ dropWhile (not.fst) progResults
-- \n->fst (program!!n)/="acc"
-- ("acc"/=).fst.(program!!)

main :: IO ()
main = do f <- openFile "input.txt" ReadMode
          c <- hGetContents f
          let program = map parseLine $ lines c
          putStr "Part 1:\n\t"
          part1 program
          putStr "Part 2:\n\t"
          part2 program
