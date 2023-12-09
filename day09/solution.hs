import System.IO
import Data.Char (isDigit, isSpace)
import Data.List (groupBy)

solution1:: String -> Int
solution1 = sum . map (predict . getNumbers) . lines

solution2:: String -> Int
solution2 = sum . map (extrapolateBackwards . getNumbers) . lines

-----------------------------------------------------------------------------

getNumbers :: String -> [Int]
getNumbers = map read . filter (/= " ") . groupBy (\a b -> (not . isSpace) a == (not . isSpace) b)

predict:: [Int] -> Int
predict xs
  | all (== 0) xs = 0
  | otherwise = last xs + predict (getDiffsOfPairs $ splitInPairs xs)

extrapolateBackwards:: [Int] -> Int
extrapolateBackwards xs
  | all (== 0) xs = 0
  | otherwise = head xs + extrapolateBackwards (map (uncurry (-)) $ splitInPairs xs)


getDiffsOfPairs:: [(Int, Int)] -> [Int]
getDiffsOfPairs = map (\a -> snd a - fst a)

splitInPairs:: [Int] -> [(Int, Int)]
splitInPairs [x] = []
splitInPairs (x:xs) = (x, head xs) : splitInPairs xs


main :: IO ()
main = do
  fileHandle <- openFile "input" ReadMode
  fileContents <- hGetContents fileHandle
  print (show $ solution1 fileContents)
  print (show $ solution2 fileContents)

  hClose fileHandle
