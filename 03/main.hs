import Data.Array(array, (!), bounds)
import Data.Bool(bool)
import System.IO(openFile, IOMode(ReadMode), hGetContents)

countTrees hill xSlope ySlope = 
    let width = (snd.snd.bounds $ hill) + 1
        height = fst.snd.bounds $ hill
    in sum $ map (hill!) $
        [ (i*ySlope, mod (i*xSlope) width) | i <- [0..div height ySlope] ]

loadHill hillList = 
    let height = length hillList
        width = length $ head hillList
        aBounds = ((0, 0), (height-1, width-1))
        cmf (y,l) = map (\(x,v)->((y,x),v)) $ zip [0..width-1] l
        aList = concatMap cmf $ zip [0..height-1] hillList
    in array aBounds aList

part1 hill = putStrLn.show $ countTrees hill 3 1

part2 hill =
    let slopes = (1,2):[ (x, 1) | x <- [1, 3..7] ]
        r = product $ map (\(x,y)->countTrees hill x y) slopes
    in putStrLn.show $ r

main = do f <- openFile "input.txt" ReadMode
          c <- hGetContents f
          let hill = loadHill $ map (map (bool 0 1.('#'==))) $ lines c
          putStr "Part 1:\n\t"
          part1 hill
          putStr "Part 2:\n\t"
          part2 hill
