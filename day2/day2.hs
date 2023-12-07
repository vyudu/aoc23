import System.IO
import Data.List

main = do
    handle <- openFile "input" ReadMode
    input <- hGetContents handle
    let rows = lines input
    putStrLn "Part 1:"
    print $ sum $ map idnum $ filter possible $ map parseGame rows
    putStrLn "Part 2:"
    print $ sum $ map (prodBalls . parseGame) rows

data Color = Red | Green | Blue deriving (Eq, Enum, Show)
data Game = Game { idnum :: Integer, balls :: [(Integer, Color)] } deriving (Show)

prodBalls :: Game -> Integer
prodBalls game =
  let 
    ball = balls game
    filterColor balls color = filter (\x -> snd x == color) balls
    [r,g,b] = map (maximum . map fst) $ map (filterColor ball) $ [Red, Green, Blue]
  in r*g*b

splitBalls :: String -> [String]
splitBalls [] = []
splitBalls balls = 
  let (sub, rest) = break (\x -> x==',' || x==';') balls 
  in sub : (splitBalls (drop 1 $ rest))
           
parseGame :: String -> Game 
parseGame str = let 
  id :: Integer = read $ takeWhile (isNumColor) $ dropWhile (not . isNumColor) str
  balls = map (ballsColors) $ splitBalls $ filter (isNumColor) $ dropWhile (/=':') str 
  in Game id balls 

ballsColors :: String -> (Integer, Color)
ballsColors str = let
  (num, letters) = break (not . isNum) str
  col = case letters of 
    "r" -> Red
    "b" -> Blue
    "gr" -> Green 
  in (read num, col)

isNumColor :: Char -> Bool
isNumColor c = c `elem` ['0'..'9'] ++ ['r','g','b',',',';']

possible :: Game -> Bool
possible game = and $ map (\(x,y) -> x <= (maxBalls y)) (balls game)

maxBalls :: Color -> Integer
maxBalls color = case color of 
  Red -> 12
  Blue -> 14
  Green -> 13

isNum :: Char -> Bool
isNum c = c `elem` ['0'..'9']
