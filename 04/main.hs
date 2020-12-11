import Data.Map(fromList, member, (!))
import Data.Maybe(isJust)
import System.IO(openFile, IOMode(ReadMode), hGetContents)
import Text.Regex(mkRegex, matchRegex, splitRegex)

isValid passport =
    let reqFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    in and $ map (flip member passport) reqFields

parsePassports c = 
    let passportStrings = splitRegex (mkRegex "\n\n") c
        passportLists =
            map (filter (/=[]).splitRegex (mkRegex " |\n")) passportStrings
        passportLists' = map (map (splitRegex $ mkRegex ":")) passportLists
        passportPairs = map (map (\(x:y:[])->(x,y))) passportLists'
    in map fromList passportPairs

part1 = putStrLn.show.length.filter isValid

part2 = 
    let res =
            [
                ("byr", mkRegex "^(19[2-9][0-9]|200[0-2])$"),
                ("iyr", mkRegex "^20(1[0-9]|20)$"),
                ("eyr", mkRegex "^20(2[0-9]|30)$"),
                ("hgt", mkRegex "^(1([5-8][0-9]|9[0-3])cm|(59|6[0-9]|7[0-6])in)$"),
                ("hcl", mkRegex "^#[0-9a-f]{6}$"),
                ("ecl", mkRegex "^(amb|blu|brn|gry|grn|hzl|oth)$"),
                ("pid", mkRegex "^[0-9]{9}$")
            ]
    in putStrLn.show.length.filter id.map (\p->and $ map (\(k, re)->isJust.matchRegex re $ p!k) res).filter isValid

main = do f <- openFile "input.txt" ReadMode
          c <- hGetContents f
          let passports = parsePassports c
          putStr "Part 1:\n\t"
          part1 passports
          putStr "Part 2:\n\t"
          part2 passports
