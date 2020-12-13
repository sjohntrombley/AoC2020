module Main where

import Control.Monad (ap)
import Data.Array (listArray, bounds, (!), (//), elems)
import Data.Bool (bool)
import Data.List ((\\), foldl')
import Data.Maybe (catMaybes)
import System.IO (IOMode(ReadMode), openFile, hGetContents)

update old new (r, c)
    | old!(r, c) == 'L' = updateEmpty r c old new
    | old!(r, c) == '#' = updateFull r c old new
    | old!(r, c) == '.' = new
    | otherwise = error "Invalid character."

updateEmpty r c old new
    = let surs = getSurroundings (fst.snd $ bounds old) (snd.snd $ bounds old) r c
      in if length (filter (isFull old) surs) == 0
         then new // [((r, c),'#')]
         else new

updateFull r c old new
    = let surs = getSurroundings (fst.snd $ bounds old) (snd.snd $ bounds old) r c
      in if length (filter (isFull old) surs) > 3
         then new // [((r, c),'L')]
         else new

isFull old loc = old!loc == '#'

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


iterRoom old 
    = let r_max = fst.snd $ bounds old
          c_max = snd.snd $ bounds old
      in foldl' (update old) old [(r, c) | r <- [0..r_max], c <- [0..c_max]]

part1 room
    = let allIters = iterate iterRoom room
          findStop = zipWith (ap ((.).(,)) (/=)) allIters $ tail allIters
      in putStrLn.show.length.filter ('#'==).elems.fst.head $ dropWhile snd findStop

--part2 :: Room -> IO ()
part2 room
    = let allIters = iterate nextRoom room
          findStop = zipWith (ap ((.).(,)) (/=)) allIters $ tail allIters
      in putStrLn.show.length.filter ('#'==).elems.fst.head $ dropWhile snd findStop

--nextRoom :: Room -> Room
nextRoom room
    = let (rmax, cmax) = snd $ bounds room
          indices = [(r, c) | r <- [0..rmax], c <- [0..cmax]]
      in room // catMaybes (map (update2 room) indices)

--update2 :: Room -> (Int, Int) -> Maybe ((Int, Int), Char)
update2 room loc
    | room!loc=='#' && numFilled room loc>4 = Just (loc, 'L')
    | room!loc=='L' && numFilled room loc==0 = Just (loc, '#')
    | otherwise = Nothing

--numFilled :: Room -> (Int, Int) -> Int
numFilled
    = let dirs = [(dr, dc) | dr <- [-1..1], dc <- [-1..1]] \\ [(0, 0)]
      in ((.) $ length.flip filter dirs).isFilledDir

--isFilledDir :: Room -> (Int, Int) -> (Int, Int) -> Bool
isFilledDir room (r, c) (dr, dc)
    = let offsets = [(r+dr*o, c+dc*o) | o <- [1..getMaxOffset room dr dc r c]]
      in foldr (ap (flip.bool.('#'==)) ('.'==).(room!)) False offsets
-- (ap (flip.bool.('#'==)) ('.'==).(room!)) loc b
--     = if room!loc == '.' then b else room!loc == '#'

-- getMaxOffset :: Room -> Int -> Int -> Int -> Int -> Int
getMaxOffset room dr dc r c
    | dr<0 && dc<0 = min r c
    | dr<0 && dc>0 = min r (cmax - c)
    | dr<0         = r
    | dr>0 && dc<0 = min (rmax - r) c
    | dr>0 && dc>0 = min (rmax - r) (cmax-c)
    | dr>0         = rmax - r
    | dc<0         = c
    | dc>0         = cmax - c
    | otherwise    = error "You aren't looking in a direction"
    where rmax = fst.snd $ bounds room
          cmax = snd.snd $ bounds room

main = do f <- openFile "input.txt" ReadMode
          c <- hGetContents f
          let room = listArray ((0,0), ((-1+).length $ lines c, (-1+).length.head $ lines c)).concat $ lines c
          putStr "Part 1:\n\t"
          part1 room
          putStr "Part 2:\n\t"
          part2 room
