import System.IO(openFile, IOMode(ReadMode), hGetContents)

parseLine line = 
    let lBound = getLBound line
        uBound = getUBound line
        policyLetter = getPolicyLetter line
        password = getPassword line
    in (lBound, uBound, policyLetter, password)

getLBound = read.takeWhile ('-'/=) 
getUBound = read.takeWhile (' '/=).tail.dropWhile ('-'/=)
getPolicyLetter = head.tail.dropWhile (' '/=) 
getPassword = drop 2.dropWhile (':'/=)

isValid1 (lBound, uBound, policyLetter, password) = 
    let count = length $ filter (policyLetter==) password
    in lBound <= count && count <= uBound

isValid2 (pos1, pos2, policyLetter, password) =
    let in1 = policyLetter == password!!(pos1-1)
        in2 = policyLetter == password!!(pos2-1)
    in in1 && not in2 || not in1 && in2

part1 passwords = putStrLn $ show $ length $ filter isValid1 passwords

part2 passwords = putStrLn $ show $ length $ filter isValid2 passwords

main = do f <- openFile "input.txt" ReadMode
          c <- hGetContents f
          let passwords = map parseLine $ lines c
          putStr "Part 1:\n\t"
          part1 passwords
          putStr "Part 2:\n\t"
          part2 passwords
