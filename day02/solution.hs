import System.IO;
import Data.List;
import Data.Char (isSpace)

solution1 :: String -> Int
solution1 = foldl (\a b -> a + head b) 0 . filter (isPossible targetIRGB) . map lineToIRGB . filter (not . null ) . splitByDelim '\n'

solution2 :: String -> Int
solution2 = sum . map ((product . tail) . lineToIRGB) . filter (not . null) . splitByDelim '\n'

-- ------------------------------------------------------------------------------


main :: IO ()
main = do
  fileHandle <- openFile "input" ReadMode
  fileContents <- hGetContents fileHandle
  print (solution1 fileContents)
  print (solution2 fileContents)
  hClose fileHandle

targetIRGB = [9999, 12, 13, 14] :: [Int]

isPossible :: [Int] -> [Int] -> Bool
isPossible target list = all (uncurry (<=)) $ zip list target

splitByDelim delim str
  | delim `elem` str = fstHalf : splitByDelim delim (tail sndHalf)
  | otherwise = [fstHalf]
  where (fstHalf, sndHalf) = break (== delim) str

getIndex:: String  -> Int -- "Game 1: *" -> 1
getIndex = read . dropWhile (not.isSpace) . takeWhile (/= ':')

getAmount:: String -> String -> Int -- "blue" "3 blue, 4 red" -> 3
getAmount t s
  | null filtered = 0
  | otherwise = read $ head $ head filtered
  where filtered = filter (\l -> last l == t) $ map (filter (not . null) . splitByDelim ' ') $ splitByDelim ',' s

getMax:: String -> String -> Int -- "blue" "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" -> 6
getMax t s = maximum $ map (getAmount t) $ splitByDelim ';' $ tail $ dropWhile (/= ':') s

lineToIRGB:: String -> [Int]
lineToIRGB s = [i, r, g, b]
      where i = getIndex s
            r = getMax "red" s
            g = getMax "green" s
            b = getMax "blue" s


