module Main where

import Data.Bool (bool)
import Data.List (foldl')
import Data.Map.Strict (Map, insert, empty)
import Numeric (showIntAtBase, readInt)
import System.IO (IOMode(ReadMode), openFile, hGetContents)
import Text.Regex (mkRegex, matchRegex)

parseIL s = let makel = matchRegex (mkRegex "^mask = ([01X]{36})$") s
                meml = matchRegex (mkRegex "^mem\\[(0|[1-9][0-9]*)] = (0|[1-9][0-9]*)$") s
            in case (makel, meml) of
                   (Just (mask:_), _) -> Left mask
                   (_, Just (addr:val:_)) -> Right (read addr, read val)
                   otherwise -> error "Malformed line in input."

part1 = print.sum.flip followInstructions (replicate 36 'X', empty)

followInstructions :: [Either String (Integer, Integer)] -> (String, Map Integer Integer) -> Map Integer Integer
followInstructions [] (mask, memory) = memory
followInstructions (Left mask:insts) state = followInstructions insts $ updateMask mask state
followInstructions (Right args:insts) state = followInstructions insts $ updateMemory args state

updateMask mask (_, memory) = (mask, memory)

updateMemory (addr, val) (mask, memory)
    = let maskedVal = masked mask val
      in (mask, insert addr maskedVal memory)

masked mask n = let sn' = showIntAtBase 2 (head.show) n ""
                    sn = replicate (36-length sn') '0'++sn'
                    sMasked = zipWith (\m c->bool m c (m=='X')) mask sn
                in fst.head $ readInt 2 (`elem`"01") (read.(:[])) sMasked

part2 = print.sum.snd.foldl' followInst2 (replicate 36 '0', empty)

followInst2 (_, memory) (Left mask) = (mask, memory)
followInst2 state (Right args) = updateMemory2 args state

updateMemory2 (addr, val) (mask, memory)
    = (mask, foldl' (\z a->insert a val z) memory $ masked2 mask addr)

masked2 mask addr
    = let addrB' = showIntAtBase 2 ("01"!!) addr ""
          addrB = replicate (36-length addrB') '0' ++ addrB'
          foldf z ('0', c) = map (c:) z
          foldf z ('1', _) = map ('1':) z
          foldf z ('X', _) = map ('0':) z ++ map ('1':) z
      in map (readInt 2 (`elem`"01") (read.(:[]))).foldl' foldf [[]] $ zip mask addrB

main = do insts <- openFile "input.txt" ReadMode >>= hGetContents >>= return.map parseIL.lines
          part1 insts
          part2 insts
          
