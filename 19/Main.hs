module Main where

import Control.Monad (ap)
import Data.Bool (bool)
import Data.Foldable (foldl')
import Data.Map.Strict (fromList, (!), insert, Map)
import Data.Maybe (isJust)
import System.IO (IOMode(ReadMode), openFile, hGetContents)
import Text.Regex (mkRegex, matchRegex)

part1 rules = print.length.filter (Just ""==).map (match rules (rules!0))

match _ _ "" = Nothing
match rules (Right c) s
    | head s == c = Just $ tail s
    | otherwise = Nothing
match _ (Left []) _ = Nothing
match rules (Left (sr:srs)) s
    = bool (match rules (Left srs) s) matchRes (isJust matchRes)
    where
        matchRes = foldl' eatRule (Just s) sr
        eatRule (Just z) x = match rules (rules!x) z
        eatRule Nothing _ = Nothing

getRule s
    | null letter && null opt2 = (read ruleNo, Left [map read $ words opt1])
    | null letter = (read ruleNo, Left $ map (map read.words) [opt1, opt2])
    | otherwise = (read ruleNo, Right $ head letter)
    where
        re = mkRegex "^([0-9]+): (([0-9 ]+)( \\| ([0-9 ]+))?|\"([a-z])\")$"
        Just [ruleNo, _, opt1, _, opt2, letter] = matchRegex re s

-- Part 2
-- At least for my input (and the example input, so I suspect everyones), rules
-- 31 and 42 both match strings of constant length (5 and 8 for both in the
-- sample input and my input, respectively). This means I only have to solve
-- for cases where that's true.
type Rules = Map Int (Either [[Int]] Char)

part2 :: Rules -> [String] -> IO ()
part2 rules
    = print.length.filter (Just ""==).map (match2 rules.eat42 rules.Just)

match2 :: Rules -> Maybe String -> Maybe String
match2 _ Nothing = Nothing
match2 rules m
    | m == Just "" = Nothing
    | m11 == Just "" = m11
    | otherwise = match2 rules (eat42 rules m)
    where m11 = match11 rules m

match11 :: Rules -> Maybe String -> Maybe String
match11 _ Nothing = Nothing
match11 rules m
    | m == Just "" = m
    | otherwise = match11 rules (eat11 rules m)

eat11 :: Rules -> Maybe String -> Maybe String
eat11 = ap ((.).eat31) eat42

eat42 :: Rules -> Maybe String -> Maybe String
eat42 _ Nothing = Nothing
eat42 rules (Just s) = match rules (rules!42) s

eat31 :: Rules -> Maybe String -> Maybe String
eat31 _ Nothing = Nothing
eat31 rules (Just s) = bool Nothing (Just prevs) (Just ""==mMatch)
    where
        (len31, _) = ruleLen rules 31
        (prevs, candidate) = splitAt (length s-len31) s
        mMatch = match rules (rules!31) candidate

ruleLen rules n
    = case rules!n of
        Left [sr] -> srSum sr
        Left [sr0, sr1]
            -> let
                (min0, max0) = srSum sr0
                (min1, max1) = srSum sr1
            in (min min0 min1, max max0 max1)
        Right c -> (1, 1)
    where 
        srSum = pairSum.unzip.map (ruleLen rules)
        pairSum (l0, l1) = (sum l0, sum l1)

main :: IO ()
main = do
        c <- openFile "input.txt" ReadMode >>= hGetContents
        let (ruleLines, "":messages) = break null $ lines c
            rules = fromList $ map getRule ruleLines
        putStr "Part 1:\n\t"
        part1 rules messages
        let rules'
                = insert
                    8
                    (Left [[42], [42, 8]])
                    (insert 11 (Left [[42, 31], [42, 11, 31]]) rules)
        putStr "Part 2:\n\t"
        part2 rules messages
