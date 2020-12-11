module Main where

import Debug.Trace (trace)

import Data.List ((\\), nub, foldl', union)
import Data.Map.Strict (member, (!), update, empty, insert, (!?), findWithDefault)
import Data.Maybe (isJust, fromJust, fromMaybe)
import System.IO (IOMode(ReadMode), openFile, hGetContents)
import Text.Regex (mkRegex, matchRegex)

getBags s =
    let res1 = fromJust $ matchRegex (mkRegex "^([a-z ]+) bags contain (.+)\\.$") s
        subBags s | isJust res2 = (res2'!!0, res2'!!1):subBags (res2'!!2)
                  | isJust res3   = [(res3'!!0, res3'!!1)]
                  | otherwise   = []
                  where
                    res2 = matchRegex (mkRegex "^([1-9][0-9]*) ([a-z ]+) bags?, (.+)$") s
                    res2' = fromJust res2
                    res3 = matchRegex (mkRegex "^([1-9][0-9]*) ([a-z ]+) bags?$") s 
                    res3' = fromJust res3
    in (res1!!0, subBags $ res1!!1)

addRLinks bags (curBag, []) = bags
addRLinks bags (curBag, ((_, subBag):subBags))
    | member subBag bags && (elem curBag $ bags!subBag)
        = addRLinks bags (curBag, subBags)
    | member subBag bags
        = addRLinks (update (Just.(curBag:)) subBag bags) (curBag, subBags)
    | otherwise
        = addRLinks (insert subBag [curBag] bags) (curBag, subBags)

buildRMap bags = foldl' (addRLinks) empty bags


getAllParents [] parents bags = parents
getAllParents (next:toDo) parents bags =
    getAllParents (toDo++((fromMaybe [] $ bags!?next)\\union parents toDo)) (next:parents) bags

part1 = putStrLn.show.(-1+).length.getAllParents ["shiny gold"] [].buildRMap

-- uncurry ((,).read) <=> \(n, sb)->(read n, sb)
buildMap bags = 
    let readFst = uncurry $ (,).read
        addBagToMap bagMap (curBag, subBags) =
            insert curBag (map readFst subBags) bagMap
    in foldl' addBagToMap empty bags

subBagCount bag bags = foldl' (\count (sbCount, subBag)->count + sbCount + sbCount*subBagCount subBag bags) 0 $ findWithDefault [] bag bags

part2 = putStrLn.show.subBagCount "shiny gold".buildMap

main :: IO ()
main = do f <- openFile "input.txt" ReadMode
          c <- hGetContents f
          let bags = map getBags $ lines c
          putStr "Part 1:\n\t"
          part1 bags
          putStr "Part 2:\n\t"
          part2 bags
