module Main where

import Control.Monad (ap)
import Data.Array (listArray, bounds, (!), (//), elems, {-debugging-} Array)
import Data.List ((\\), foldl')
import System.IO (IOMode(ReadMode), openFile, hGetContents)

type Room = Array (Int, Int) Char

update :: Room -> Room -> (Int, Int) -> Room
update old new (r, c)
    | old!(r, c) == 'L' = updateEmpty r c old new
    | old!(r, c) == '#' = updateFull r c old new
    | old!(r, c) == '.' = new
    | otherwise = error "Invalid character."

updateEmpty :: Int -> Int -> Room -> Room -> Room 
updateEmpty r c old new
    = let surs = getSurroundings (fst.snd $ bounds old) (snd.snd $ bounds old) r c
      in if length (filter (isFull old) surs) == 0
         then new // [((r, c),'#')]
         else new

updateFull :: Int -> Int -> Room -> Room -> Room 
updateFull r c old new
    = let surs = getSurroundings (fst.snd $ bounds old) (snd.snd $ bounds old) r c
      in if length (filter (isFull old) surs) > 3
         then new // [((r, c),'L')]
         else new

isFull :: Room -> (Int, Int) -> Bool
isFull old loc = old!loc == '#'

getSurroundings :: Int -> Int -> Int -> Int -> [(Int, Int)]
getSurroundings r_max c_max r c 
    | (r, c) == (0, 0) = [(r', c') | r' <- [0..1], c' <- [0..1]] \\ [(r, c)]
    | (r, c) == (0, c_max) = [(r', c') | r' <- [0..1], c' <- [c-1..c]] \\ [(r, c)]
    | (r, c) == (r_max, 0) = [(r', c') | r' <- [r-1..r], c' <- [0..1]] \\ [(r, c)]
    | (r, c) == (r_max, c_max) = [(r', c') | r' <- [r-1..r], c' <- [c-1..c]] \\ [(r, c)]
    | r == 0 = [(r', c') | r' <- [0..1], c' <- [c-1..c+1]] \\ [(r, c)]
    | r == r_max = [(r', c') | r' <- [r-1..r], c'<-[c-1..c+1]] \\ [(r, c)]
    | c == 0 = [(r', c') | r' <- [r-1..r+1], c' <- [0..1]] \\ [(r, c)]
    | c == c_max = [(r', c') | r' <- [r-1..r+1], c'<-[c-1..c]] \\ [(r, c)]
    | otherwise = [(r', c') | r' <- [r-1..r+1], c'<- [c-1..c+1]] \\ [(r, c)]


iterRoom :: Room -> Room
iterRoom old 
    = let r_max = fst.snd $ bounds old
          c_max = snd.snd $ bounds old
      in foldl' (update old) old [(r, c) | r <- [0..r_max], c <- [0..c_max]]

part1 :: Room -> IO ()
part1 room
    = let allIters = iterate iterRoom room
          findStop = zipWith (ap ((.).(,)) (/=)) allIters $ tail allIters
      in putStrLn.show.length.filter ('#'==).elems.fst.head $ dropWhile snd findStop

main = do f <- openFile "input.txt" ReadMode
          c <- hGetContents f
          let room = listArray ((0,0), ((-1+).length $ lines c, (-1+).length.head $ lines c)).concat $ lines c
          putStrLn $ show room
          putStr "Part 1:\n\t"
          part1 room
