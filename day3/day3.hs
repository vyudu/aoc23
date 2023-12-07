import System.IO
import Data.List

main = do
    handle <- openFile "input" ReadMode
    contents <- hGetContents handle
    putStrLn "Part 1:"

    putStrLn "Part 2:"

data Symbols = Num Integer | Symbol deriving (Eq, Show)
data Point = Point {x :: Integer, y :: Integer} deriving (Eq, Show) 

readNums :: [String] -> [(Symbols, Point)]
readNums input = map (\x -> findNums) input

findNums :: String -> (Integer, Integer)

len :: Symbols -> Integer
len (Num a) = length (show a) 
len Sym = 1

getSurr :: (Symbols, Point) -> (Point, Point) 
getSurr (sym, point) =
  let (x,y) = (x point, y point)
      l = len sym 
  in (Point (x-1) (y-1), Point (x + l), (y + 1))
