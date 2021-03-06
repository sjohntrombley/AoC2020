module Main where

import Data.Bool (bool)
import Data.List ((\\), foldl', transpose, nub, partition)
import Data.Maybe (catMaybes, isJust)
import System.IO (IOMode(ReadMode), hGetContents, openFile)
import Text.Regex (matchRegex, mkRegex, splitRegex)

getErrorRate classes
    = let classRanges = getRanges classes
          isNotValidField n
              = all (\(lb, ub)->n<lb || ub<n) classRanges
      in sum.filter isNotValidField.concat

getRanges = concatMap (\(_,b0,b1)->[b0, b1])

-- Part 2 stuff
part2 classes myTic nTics
    = let Just fields = (\x->getFields x) $ fieldPosses classes $ myTic:nTics
          -- c2mi = class to maybe i
          c2mi = (\c i->if take 9 c=="departure" then Just i else Nothing)
          ansIndices = catMaybes $ zipWith c2mi (concat fields) [0..length fields-1]
      in print.product $ map (toInteger.(myTic!!)) ansIndices

filterValid classes
    = let classRanges = getRanges classes
          isValidField n
              = any (\(lb, ub)->lb<=n && n<=ub) classRanges
      in filter (all isValidField)

fieldPosses classes tickets
    = let -- returns a list of possible classes for a field
          fieldList vals = foldl' (addField vals) [] classes
          -- add the class to cs if every value in this field could be a member
          -- of the class
          addField vals cs (c, b0, b1)
              = if possibleField b0 b1 vals then c:cs else cs
          -- True if all values in this field 
          possibleField (lb0, ub0) (lb1, ub1) vals
              = all (\n->lb0<=n&&n<=ub0 || lb1<=n&&n<=ub1) vals
          unfilteredAns = map (fieldList).transpose $ filterValid classes tickets
      in condensePosses unfilteredAns

condensePosses pcs
    | any (flip any (concat singles).flip elem) multis
        = condensePosses $ map (\l->bool (l\\concat singles) l $ length l==1) pcs
    | otherwise = pcs
    where (singles, multis) = partition ((1==).length) pcs

getFields pcs
    | any ((0==).length) pcs = Nothing
    | all ((1==).length) pcs = bool Nothing (Just pcs) $ pcs==nub pcs
    | otherwise
        = let (singles, nonsing:rest) = span ((1==).length) pcs
              guess = take 1 nonsing
              notGuess = tail nonsing
              mAnswer = getFields $ singles ++ guess:map (\\guess) rest
          in if isJust mAnswer then mAnswer
             else getFields $ singles ++ notGuess:rest

-- input parsing
parseInput s
    = let [classesStr, myStr, nearbyStr] = splitRegex (mkRegex "\n\n") s
          classes = map parseClass $ lines classesStr
          myTicket = read $ "["++(lines myStr!!1)++"]"
          nearbyTickets = map (\t->read $ "["++t++"]").tail $ lines nearbyStr
      in (classes, myTicket, nearbyTickets)

parseClass s
    = let re = mkRegex "^(.+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)$"
          Just [className, lb0, ub0, lb1, ub1] = matchRegex re s
      in (className, (read lb0, read ub0), (read lb1, read ub1))


main = do c <- openFile "input.txt" ReadMode >>= hGetContents
          let (classes, myTicket, nearbyTickets) = parseInput c
          putStr "Part 1:\n\t"
          print $ getErrorRate classes nearbyTickets
          putStr "Part 2:\n\t"
          part2 classes myTicket nearbyTickets
