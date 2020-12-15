module Main where

import Data.Bool (bool)
import Data.List (minimumBy, foldl1')
import System.Environment (getArgs)
import System.IO (IOMode(ReadMode), openFile, hGetContents)
import Text.Regex (splitRegex, mkRegex)

part1 t bs = let b = soonestBus t bs in print $ b * (b - mod t b)

soonestBus t bs = minimumBy (compareBusses t) bs

compareBusses t b1 b2
    | mod t b1==0 && mod t b2==0 = EQ
    | mod t b1 == 0              = LT
    | mod t b2 == 0              = GT
    | otherwise                  = compare (b1 - mod t b1) (b2 - mod t b2)

part2 :: [(Integer, Integer)] -> IO ()
part2 = print.uncurry mod.combineMods

--combineMods :: [(Integer, Integer)] -> (Integer, Integer)
combineMods
    = let foldf x@(_, b0) y@(_, b1) = bool (foldf' y x) (foldf' x y) (b0>b1)
          foldf' (e0, b0) (e1, b1) = let n = rem (modInv ((b0, 0), (b1, 1))*(e0-e1)) b0
                                     in (rem (n*b1 + e1) (b0*b1), b0*b1)
      in foldl1' foldf

--modInv :: ((Integer, Integer), (Integer, Integer)) -> Integer
modInv ((_, t), (0, _)) = t
modInv ((r, t), (r', t')) = modInv ((r', t'), (rem r r', t - quot r r'*t'))

main = do fn <- getArgs>>=return.head
          f <- openFile fn ReadMode
          c <- hGetContents f >>= return.lines
          let time = read $ head c
          let busStrs = splitRegex (mkRegex ",") $ c!!1
          let busses = map read $ filter ("x"/=) busStrs
          putStr "Part 1:\n\t" 
          part1 time busses
          -- make it look like (c, n) where t is congruent with c (mod n)
          let schedule = map (\(x, y)->(-x, read y)).filter (("x"/=).snd) $ zip (map toInteger [0..length busStrs-1]) busStrs
          putStr "Part 2:\n\t"
          part2 schedule

