module Day1 where

import Utilities
import Data.List.Split

import Data.Text(pack, unpack, replace, isInfixOf)

replaceGood :: String -> String -> String -> String
replaceGood a b s = unpack ((replace (pack a) (pack b) (pack s)))

-- Remove all non-numeric characters from string
removeNonNumeric :: String -> String
removeNonNumeric = filter (`elem` ['0'..'9'])

isNumeric :: Char -> Bool
isNumeric s = or [s == '0', s == '1', s == '2', s == '3', s == '4', s == '5', s == '6', s == '7', s == '8', s == '9']

-- Get first and last characters of string
getNum :: String -> Int
getNum s = read ((head (removeNonNumeric s)) : [last (removeNonNumeric s)]) :: Int

-- Replace "one" with "1", "two" with "2", etc.
replaceWords :: String -> String
replaceWords s = replaceGood "nine" "9" (replaceGood "eight" "8" (replaceGood "seven" "7" (replaceGood "six" "6" (replaceGood "five" "5" (replaceGood "four" "4" (replaceGood "three" "3" (replaceGood "two" "2" (replaceGood "one" "1" s))))))))

iterLeft :: String -> String -> String
iterLeft [] s = s
iterLeft (x:xs) s = if checkWord s then (replaceWords s ++ [x] ++ xs)
                    else if isNumeric x then (s ++ [x] ++ xs)
                    else iterLeft xs (s ++ [x])

iterBoth s = iterLeft (iterRight s "") ""

rcons xs x = xs ++ [x]

iterRight :: String -> String -> String
iterRight [] s = s
iterRight (xs_) s = 
    let x = last xs_
        xs = init xs_ in
        if checkWord s then (xs ++ [x] ++ replaceWords s) 
        else if isNumeric x then (xs ++ [x] ++ s)
        else iterRight xs ([x] ++ s)

-- Check to see if string contains one of "one" through "nine" as a substring
checkWord :: String -> Bool
checkWord s = or [isInfixOf (pack "one") (pack s), isInfixOf (pack "two") (pack s), isInfixOf (pack "three") (pack s), isInfixOf (pack "four") (pack s), isInfixOf (pack "five") (pack s), isInfixOf (pack "six") (pack s), isInfixOf (pack "seven") (pack s), isInfixOf (pack "eight") (pack s), isInfixOf (pack "nine") (pack s)]

part1 = do
    lines <- getLines "day1/input.txt"
    putStrLn $ show $ sum $ map getNum lines

part2 = do
    lines <- getLines "day1/input.txt"
    putStrLn $ show $ sum $ map (getNum . iterBoth) lines