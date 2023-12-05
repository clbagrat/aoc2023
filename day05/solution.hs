import Data.List (find, groupBy)
import Distribution.Compat.Prelude (fromMaybe, isDigit)
import System.IO

solution1 :: String -> Int
solution1 input = minimum $ map (\s -> foldl mapFold s maps) seeds
  where
    inputLines = lines input
    seeds = getNumbers $ head inputLines
    maps = getMaps (tail inputLines)

solution2 :: String -> Int
solution2 input = minimum $ map fst $ foldl mapFoldRanges seeds maps
  where
    inputLines = lines input
    seeds = toRanges $ getNumbers $ head inputLines
    maps = getMaps (tail inputLines)

-----------------

toRanges :: [Int] -> [(Int, Int)]
toRanges xs = map (\a -> (head a, head a + a !! 1 - 1)) $ chunksOf2 xs []

chunksOf2 :: [Int] -> [[Int]] -> [[Int]]
chunksOf2 [] res = reverse res
chunksOf2 xs res = chunksOf2 (tail $ tail xs) (take 2 xs : res)

getNumbers :: String -> [Int]
getNumbers = map read . filter (\a -> not (null a) && isDigit (head a)) . groupBy (\a b -> isDigit a == isDigit b)

getMaps :: [String] -> [[(Int, Int, Int)]]
getMaps = map (map toRange) . filter (not . null . head) . groupBy (\a b -> (not . null) a == (not . null) b) . filter (\a -> null a || isDigit (head a))

toRange :: String -> (Int, Int, Int)
toRange = toTuple . getNumbers
  where
    toTuple [x, y, z] = (x, y, y + z)

mapFold :: Int -> [(Int, Int, Int)] -> Int
mapFold seed maps = destinationStart + (seed - sourceStart)
  where
    targetMap = fromMaybe (0, 0, 0) $ find (\(f, s, t) -> seed >= s && seed <= t) maps
    (destinationStart, sourceStart, _) = targetMap

mapFoldRanges :: [(Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int)]
mapFoldRanges xs ms = uncurry (++) computed
  where
    computed = foldl magicFunc ([], xs) ms

magicFunc :: ([(Int, Int)], [(Int, Int)]) -> (Int, Int, Int) -> ([(Int, Int)], [(Int, Int)])
magicFunc acc m = (distributed ++ newDistributed, newLeftOvers)
  where
    leftOvers = snd acc
    distributed = fst acc
    newPairs = map (pair m) leftOvers
    newDistributed = concatMap fst newPairs
    newLeftOvers = concatMap snd newPairs

pair :: (Int, Int, Int) -> (Int, Int) -> ([(Int, Int)], [(Int, Int)])
pair (m0, m1, m2) (x1, x2) = (map (\a -> (m0 + (fst a - m1), m0 + (snd a - m1))) $ getIntersections (m1, m2) (x1, x2), getDifferences (m1, m2) (x1, x2))

getIntersections :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
getIntersections (startB, endB) (startA, endA)
  | startA <= endB && startB <= endA = [(max startA startB, min endA endB)]
  | otherwise = []

getDifferences :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
getDifferences (startB, endB) (startA, endA)
  | endA <= startB || startA >= endB = [(startA, endA)] -- No intersection
  | startA < startB && endA <= endB = [(startA, startB)] -- Range A before Range B
  | startA >= startB && endA > endB = [(endB, endA)] -- Range A after Range B
  | startA < startB && endA > endB = [(startA, startB), (endB, endA)] -- Range A surrounding Range B
  | otherwise = [] -- Range B completely within Range A

main :: IO ()
main = do
  fileHandle <- openFile "input" ReadMode
  fileContents <- hGetContents fileHandle
  print (show $ solution1 fileContents)
  print (show $ solution2 fileContents)

  hClose fileHandle
