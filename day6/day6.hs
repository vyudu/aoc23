import System.IO
import Data.List

main = do
    handle <- openFile "input" ReadMode
    contents <- hGetContents handle
    putStrLn "Part 1:"

    putStrLn "Part 2:"
