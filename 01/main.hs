import System.IO(openFile, IOMode(..), hGetContents)
import Data.Maybe(isNothing, fromJust)

findPairProduct _ [_] = Nothing
findPairProduct n (x:xs) = if x > n then findPairProduct n xs else
    let y=n-x
    in if elem y xs then Just $ x * y else findPairProduct n xs

findTripleProduct _ [_, _] = Nothing
findTripleProduct n (x:xs) = if x > n then findTripleProduct n xs else
    let m=n-x
        r=findPairProduct m xs
    in if isNothing r then findTripleProduct n xs else Just $ x*(fromJust r)

part1 = do f <- openFile "input.txt" ReadMode
           c <- hGetContents f
           let r = findPairProduct 2020 $ map read $ lines c
           if isNothing r
               then putStrLn "No pair of numbers adds to 2020."
               else putStrLn $ show $ fromJust r

part2 = do f <- openFile "input.txt" ReadMode
           c <- hGetContents f
           let r = findTripleProduct 2020 $ map read $ lines c
           if isNothing r
               then putStrLn "No triple of numbers adds to 2020."
               else putStrLn $ show $ fromJust r

main = do putStr "Part 1:\n\t"
          part1
          putStr "Part 2:\n\t"
          part2
