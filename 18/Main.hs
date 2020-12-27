module Main where

import Debug.Trace (trace)

import Data.Bool (bool)
import Data.Foldable (foldl')
import System.IO(IOMode(ReadMode), openFile, hGetContents)

-- expr := int | pExpr | opExpr
-- pExpr := (expr)
-- opExpr := expr op arg
-- op := + | *
-- arg := int | pExpr

part1 = print.sum.map (parseExpr.tokenize).lines

parseExpr [] = error "parseExpr: Empty list"
parseExpr ts
    | length ts == 1 = read $ head ts
    | last ts == ")" && null rest = parsePExpr ts
    | otherwise = parseOpExpr ts
    where (rest, _) = getPExpr ts

parsePExpr ts = parseExpr.tail $ init ts

parseOpExpr ts
    | last ts == ")" = case last rest of
                        "+" -> parseExpr (init rest) + parsePExpr pExpr
                        "*" -> parseExpr (init rest) * parsePExpr pExpr
                        otherwise -> error $ "parseOpExpr:\ntokens: " ++ show ts ++ "\nrest: " ++ show rest ++ "\npe: " ++ show pExpr ++ "\n\n"
    | otherwise = case last $ init ts of
                    "+" -> parseExpr (init $ init ts) + read (last ts)
                    "*" -> parseExpr (init $ init ts) * read (last ts)
                    otherwise -> error $ "parseOpExpr:\ntokens: " ++ show ts ++ "\n\n"
    where (rest, pExpr) = getPExpr ts

getPExpr ts = gpe 1 [")"].tail.reverse $ ts
    where
        gpe 0 pe rest = (reverse rest, pe)
        gpe n pe (")":rest) = gpe (n+1) (")":pe) rest
        gpe n pe ("(":rest) = gpe (n-1) ("(":pe) rest
        gpe n pe (t:rest) = gpe n (t:pe) rest
        gpe _ _ _ = error $ "getPExpr:\ntokens: " ++ show ts ++ "\n\n"

tokenize "" = []
tokenize s
    = case s of
        ('(':s') -> "(":tokenize (eatSpaces s')
        (')':s') -> ")":tokenize (eatSpaces s')
        otherwise -> uncurry ((.tokenize.eatSpaces).(:)) (span (`notElem`" )") s)

eatSpaces "" = ""
eatSpaces (c:s) = bool (c:s) (eatSpaces s) (c==' ')

-- Part 2
part2 = print.sum.map (parseExpr2.tokenize).lines

--parseExpr2 :: Num a => [String] -> a
parseExpr2 ts
    | length ts == 1 = read $ head ts
    | last ts==")" && null rest = parseExpr2.tail $ init ts
    | isMExpr 0 ts = parseMulExpr ts
    | otherwise = parsePlusExpr ts
    where (rest, _) = getPExpr ts

parseMulExpr ts = parseExpr2 rest * parseMRightArg rightArg
    where (rest, rightArg) = splitMExpr ts

parseMRightArg ts
    | length ts == 1 = read $ head ts
    | last ts==")" && null rest = parseExpr2.tail $ init ts
    | otherwise = parsePlusExpr ts
    where (rest, _) = getPExpr ts

parsePlusExpr ts
    | last (init ts) == "+"
        = parseExpr2 (take (length ts-2) ts) + read (last ts)
    | last ts == ")" = parseExpr2 (init rest) + parseExpr2 (tail $ init pe)
    where (rest, pe) = getPExpr ts

splitMExpr = sme 0 [].reverse
    where
        sme n rightArg (")":rest) = sme (n+1) (")":rightArg) rest
        sme n rightArg ("(":rest) = sme (n-1) ("(":rightArg) rest
        sme 0 rightArg ("*":rest) = (reverse rest, rightArg)
        sme n rightArg (t:rest) = sme n (t:rightArg) rest

isMExpr _ [] = False
isMExpr n ("(":ts) = isMExpr (n+1) ts
isMExpr n (")":ts) = isMExpr (n-1) ts
isMExpr 0 ("*":ts) = True
isMExpr n (_:ts) = isMExpr n ts

main = do
        c <- openFile "input.txt" ReadMode >>= hGetContents
        putStr "Part 1:\n\t"
        part1 c
        putStr "Part 2:\n\t"
        part2 c
