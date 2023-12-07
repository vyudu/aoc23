import System.IO
import Control.Monad
import Data.List

main = do 
    input <- getContents
    let rows = lines input
    print $ sum $ map (readInt . getTwo . filter (\x -> x `elem` ['1'..'9'])) $ rows 
    print $ sum $ map (readInt . getTwo . filter (\x -> x `elem` ['1'..'9']) . replace) $ rows 

getTwo :: String -> String
getTwo x = [head x, last x]

readInt :: String -> Integer
readInt a = read a :: Integer

digits = [(0, "zero"), (1, "one"), (2, "two"), (3, "three"), (4, "four"), (5, "five"), (6, "six"), (7, "seven"), (8, "eight"), (9, "nine")]

replace :: String -> String
replace str = reverse (strToNum True $ reverse (strToNum False str))

strToNum :: Bool -> String -> String
strToNum rev [] = []
strToNum rev str@(x:xs)
  | null ls   = x:(strToNum rev xs) 
  | otherwise = let [(num, word)] = ls in (show num) ++ (drop (length word - 1) str)  
  where substr = take 5 str
        ls = findNum rev substr

findNum :: Bool -> String -> [(Integer, String)] 
findNum rev substr
  | rev       = filter ((\x -> x `isPrefixOf` substr) . reverse . snd) $ digits
  | otherwise = filter ((\x -> x `isPrefixOf` substr) . snd) $ digits
