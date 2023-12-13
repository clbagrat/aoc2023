import Data.Array
import Data.Char
import Data.List
import System.IO

solution1 :: String -> Int
solution1 = sum . map getArrangementsDP . lines

solution2 :: String -> Int
solution2 = sum . map (getArrangementsDP . replicateFive) . lines

-------------------

getArrangementsDP :: String -> Int
getArrangementsDP line = dp springs rules
  where
    (springs, rs) = break isSpace line
    rules = getNumbers rs

dp :: String -> [Int] -> Int
dp springs blocks = d 0 0 0 -- SpringIndex BlockIndex CurrentAmountOf#
  where
    (sl, bl) = (length springs, length blocks)
    (si, bi) = (sl - 1, bl - 1)
    s' = listArray (0, sl) springs
    b' = listArray (0, bi) blocks

    d s b c
      | s == sl && b == bi && c == b' ! b = 1
      | s == sl && b == bl && c == 0 = 1
      | s == sl = 0
      | s' ! s == '?' = sum [rd s b c, rh s b c ]
      | s' ! s == '#' = rh s b c
      | otherwise = rd s b c

    rd s b c
      | c == 0 = ds ! (s + 1, b, 0)
      | c > 0 && b < bl && c == b' ! b = ds ! (s + 1, b + 1, 0)
      | otherwise = 0
    rh s b c = ds ! (s + 1, b, c + 1)

    ds = listArray bounds [d s b c | (s, b, c) <- range bounds]
    bounds = ((0, 0, 0), (sl, bl, sl))

getNumbers :: String -> [Int]
getNumbers = map read . filter (isDigit . head) . groupBy (\a b -> isDigit a == isDigit b)

replicateFive :: String -> String
replicateFive =  (\(a, b) -> intercalate "?" (replicate 5 a) ++ " " ++ intercalate "," (replicate 5 $ tail b)) . break isSpace


main :: IO ()
main = do
  fileHandle <- openFile "input" ReadMode
  fileContents <- hGetContents fileHandle
  print (show $ solution1 fileContents)
  print (show $ solution2 fileContents)

  hClose fileHandle
