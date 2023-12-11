import System.IO
import Data.List
import Data.Char

main = do
    handle <- openFile "input" ReadMode
    contents <- hGetContents handle 
    let rows = lines contents
    putStrLn "Part 1:"
    print $ sum $ liveNums rows 
    putStrLn "Part 2:"
    let nums = findNums rows
    print $ sum $ map (gearRatio nums) $ findGears rows

data Point = Point {r :: Int, c :: Int} deriving (Eq, Show) 
data Symbol = Sym | Num Int deriving (Eq, Show)

liveNums:: [String] -> [Int]
liveNums input = map fst $ filter (\x -> any (touching x) syms) nums 
  where nums = findNums input
        syms = findSyms input

findNums :: [String] -> [(Int, Point)]
findNums strs = concat $ map getPoints $ zip [0..] $ map (readInts . zip [0..]) strs 
  where getPoints (r, vals) = zip (map fst vals) (map (Point r . snd) vals)
        readInts :: [(Int, Char)] -> [(Int, Int)]
        readInts idxStr = 
          case span (isDigit . snd) idxStr of
            ([], []) -> []
            ([], x:xs) -> readInts xs 
            (digits, xs) -> (read (map snd digits), fst $ head digits) : readInts xs 

findSyms :: [String] -> [(Int, Point)]
findSyms strs = zip (repeat 1) $ concat $ map (\(r, cs) -> map (Point r) cs) $ zip [0..] $ map cValues strs
  where isSym = \x -> (x /= '.' && (not $ isDigit x)) 
        cValues str = map snd $ filter (isSym . fst) $ zip str [0..]

findGears :: [String] -> [(Int, Point)]
findGears strs = zip (repeat 1) $ concat $ map (\(r, cs) -> map (Point r) cs) $ zip [0..] $ map cValues strs
  where isGear x = x == '*'
        cValues str = map snd $ filter (isGear . fst) $ zip str [0..]

gearRatio :: [(Int, Point)] -> (Int, Point) -> Int
gearRatio nums gp = case length nNums of
    2 -> (fst $ head nNums) * (fst $ last nNums) 
    otherwise -> 0
  where nNums = filter (\ip -> touching ip gp) nums 

-- assume integer is the first argument
touching :: (Int, Point) -> (Int, Point) -> Bool
touching (i1,p1) (i2,p2) = (r2, c2) `elem` surr 
   where (l1,l2) = (length $ show i1, length $ show i2)
         surr = [(ri,ci) | ri <- [(r1-1)..(r1+1)], ci <- [(c1-1)..(c1+l1)]]
         (r1, c1, r2, c2) = (r p1, c p1, r p2, c p2)


