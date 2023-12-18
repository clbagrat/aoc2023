import Data.Char (digitToInt, isDigit)
import Data.Map qualified as Map
import System.IO

solution1:: String -> Int
solution1 input = area th + (perimeter th `div` 2 + 1)
  where
    th = foldl thread [] $ map extractDirection $ lines input

solution2:: String -> Int
solution2 input = area th + (perimeter th `div` 2 + 1)
  where
    th = foldl thread [] $ map extractDirectionFromHex $ lines input

---------------------------------

area :: [Coord] -> Int
area p = abs (sum [x0 * y1 - x1 * y0 | ((x0, y0), (x1, y1)) <- segments p]) `div` 2

perimeter p = abs (sum [abs (x0 - x1) + abs (y0 - y1) | ((x0, y0), (x1, y1)) <- segments p])

segments :: [Coord] -> [(Coord, Coord)]
segments p = zip p (tail p ++ [head p])

thread :: [Coord] -> (Coord, Int) -> [Coord]
thread [] ((x, y), l) = [(x * l, y * l)]
thread ((px, py) : xs) ((x, y), l) = (px + x * l, py + y * l) : (px, py) : xs

extractDirection :: String -> (Coord, Int)
extractDirection ('R' : _ : xs) = ((1, 0), getNumbers xs)
extractDirection ('L' : _ : xs) = ((-1, 0), getNumbers xs)
extractDirection ('U' : _ : xs) = ((0, -1), getNumbers xs)
extractDirection ('D' : _ : xs) = ((0, 1), getNumbers xs)

getNumbers :: String -> Int
getNumbers = read . takeWhile isDigit

extractDirectionFromHex :: String -> (Coord, Int)
extractDirectionFromHex line = case hex !! 7 of
  '0' -> ((1, 0), fromHex)
  '1' -> ((0, 1), fromHex)
  '2' -> ((-1, 0), fromHex)
  '3' -> ((0, -1), fromHex)
  where
    hex = words line !! 2
    fromHex = foldl (\x -> ((16 * x) +) . digitToInt) 0 (take 5 (drop 2 hex))

type Coord = (Int, Int)

main :: IO ()
main = do
  fileHandle <- openFile "input" ReadMode
  fileContents <- hGetContents fileHandle
  print (show $ solution1 fileContents)
  print (show $ solution2 fileContents)

  hClose fileHandle
