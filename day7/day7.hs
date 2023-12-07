import System.IO
import Data.List
import qualified Data.Map as Map

main = do
    handle <- openFile "input" ReadMode
    contents <- hGetContents handle
    let rows = lines contents
    putStrLn "Part 1:"
    let sortedHands = sortBy compareHands $ map toHand rows 
    print $ sum $ zipWith (*) (map bet $ sortedHands) [1..length sortedHands] 
    putStrLn "Part 2:"
    let sortedHands = sortBy compareHands $ map toJokerHand rows
    print $ sum $ zipWith (*) (map bet $ sortedHands) [1..length sortedHands] 

data Type = HighCard | OnePair | TwoPair | Three | FullHouse | Four | Five deriving (Enum, Eq, Ord, Show)
data Hand = Cards { cards :: String, strength :: Type, bet :: Int } deriving (Show, Eq, Ord)

handType :: [Int] -> Type
handType nums = case nums of  
  [1,1,1,1,1] -> HighCard
  [1,1,1,2] -> OnePair
  [1,2,2] -> TwoPair
  [1,1,3] -> Three
  [2,3] -> FullHouse
  [1,4] -> Four 
  [5] -> Five

toHand :: String -> Hand
toHand str = 
  let (cards, bet) = break (==' ') str
      nums = sort $ Map.elems $ Map.fromListWith (+) $ [(c,1) | c <- cards]
  in Cards { cards = cards, strength = handType nums, bet = read bet } 

toJokerHand str = 
  let (cards, bet) = break (==' ') str
      charDict = Map.fromListWith (+) $ [(c,1) | c <- cards]
      nums = sort $ Map.elems $ charDict
      jokerNums = case Map.lookup 'J' charDict of 
         Nothing -> nums 
         Just 5 -> nums
         Just x -> init (delete x nums) ++ [(last (delete x nums) + x)]
  in Cards { cards = cards, strength = handType jokerNums, bet = read bet } 

compareHands :: Hand -> Hand -> Ordering
compareHands h1 h2 
  | strength h1 > strength h2 = GT
  | strength h1 < strength h2 = LT
  | otherwise                 = cardOrder (cards h1) (cards h2)

cardOrder :: String -> String -> Ordering
cardOrder [] [] = EQ
cardOrder (x:xs) (y:ys) = case compare (rank x) (rank y) of
  EQ -> cardOrder xs ys 
  GT -> GT 
  LT -> LT
  where
    rank :: Char -> Int
    rank x = case x of 
      'A' -> 14
      'K' -> 13
      'Q' -> 12
      'J' -> 1
      'T' -> 10
      otherwise -> read [x] 
