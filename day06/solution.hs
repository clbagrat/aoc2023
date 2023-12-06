import Data.List (find, groupBy)
import Distribution.Compat.Prelude (fromMaybe, isDigit, isSpace)
import System.IO

solution1:: String -> Int
solution1 = product . map getWinsAmount . mergeToTuples . map getNumbers . lines

solution2:: String -> Int
solution2 = head . map getWinsAmount . mergeToTuples . map (getNumbers . removeSpaces) . lines

-----------------------------------------------

removeSpaces:: String -> String
removeSpaces = filter (not . isSpace)

getWinsAmount :: (Int, Int) -> Int
getWinsAmount (t, maxDistance) = length $ filter (>maxDistance) $ map (calcDistance t) [0..t]

calcDistance :: Int -> Int -> Int
calcDistance maxTime holdTime = holdTime * (maxTime - holdTime)

mergeToTuples :: [[Int]] -> [(Int, Int)]
mergeToTuples xs = zip (head xs) (last xs)

getNumbers :: String -> [Int]
getNumbers = map read . filter (\a -> not (null a) && isDigit (head a)) . groupBy (\a b -> isDigit a == isDigit b)

main :: IO ()
main = do
  fileHandle <- openFile "input" ReadMode
  fileContents <- hGetContents fileHandle
  print (show $ solution1 fileContents)
  print (show $ solution2 fileContents)

  hClose fileHandle
