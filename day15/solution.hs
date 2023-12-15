import Data.Char (digitToInt, isDigit, isLetter, ord)
import Data.List (findIndex, splitAt)
import Data.Map (Map, empty, fromList, insert, lookup, mapWithKey, member)
import Data.Maybe (fromJust, isJust)
import System.IO

solution1 :: String -> Int
solution1 = sum . map hash . splitByDelim ',' . filter (/= '\n')

solution2 :: String -> Int
solution2 = sum . mapWithKey calcSingleBox . toBoxes Data.Map.empty . splitByDelim ',' . filter (/= '\n')

-----------------------------------

toBoxes :: Map Int [(String, Int)] -> [String] -> Map Int [(String, Int)]
toBoxes m [] = m
toBoxes m (x : xs)
  | member h m = toBoxes (insert h (operateBox (fromJust $ Data.Map.lookup h m) l p isD) m) xs
  | otherwise = toBoxes (insert h [(l, p)] m) xs
  where
    l = takeWhile isLetter x
    h = hash l
    isD = last x == '-'
    p = if isDigit (last x) then digitToInt (last x) else 0

operateBox :: [(String, Int)] -> String -> Int -> Bool -> [(String, Int)]
operateBox b l p True = filter (\(a, _) -> a /= l) b
operateBox b l p _
  | isJust mIndex = replaceNth (fromJust mIndex) (l, p) b
  | otherwise = b ++ [(l, p)]
  where
    mIndex = findIndex (\(a, _) -> a == l) b

calcSingleBox :: Int -> [(String, Int)] -> Int
calcSingleBox i b = foldl (\acc ((_, p), li) -> acc + (i + 1) * li * p) 0 $ zip b [1 ..]

hash :: String -> Int
hash = foldl (\acc a -> ((acc + ord a) * 17) `mod` 256) 0

splitByDelim :: Char -> String -> [String]
splitByDelim delim str
  | delim `elem` str = fstHalf : splitByDelim delim (tail sndHalf)
  | otherwise = [fstHalf]
  where
    (fstHalf, sndHalf) = break (== delim) str

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x : xs)
  | n == 0 = newVal : xs
  | otherwise = x : replaceNth (n - 1) newVal xs

main :: IO ()
main = do
  fileHandle <- openFile "input" ReadMode
  fileContents <- hGetContents fileHandle
  print (show $ solution1 fileContents)
  print (show $ solution2 fileContents)

  hClose fileHandle
