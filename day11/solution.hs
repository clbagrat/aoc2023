import Data.List
import System.IO

solution1 :: String -> Int
solution1 input = sum $ map calcManDistance $ getPairs $ adjustToExpansion 1 expandedX expandedY $ getGalaxies lineLength input
  where
    lineLength = length (takeWhile (/= '\n') input) + 1
    expandedY = getExpandedY $ lines input
    expandedX = getExpandedX $ lines input

solution2 :: String -> Int
solution2 input = sum $ map calcManDistance $ getPairs $ adjustToExpansion (1000000 - 1) expandedX expandedY $ getGalaxies lineLength input
  where
    lineLength = length (takeWhile (/= '\n') input) + 1
    expandedY = getExpandedY $ lines input
    expandedX = getExpandedX $ lines input

-----------------------

calcManDistance :: (Coord, Coord) -> Int
calcManDistance (a, b) = abs (bx - ax) + abs (by - ay)
  where
    (ax, ay) = a
    (bx, by) = b

getPairs :: [Coord] -> [(Coord, Coord)]
getPairs xs = [(x, y) | (x : ys) <- tails xs, y <- ys]

adjustToExpansion :: Int -> [Int] -> [Int] -> [Coord] -> [Coord]
adjustToExpansion k xs ys = map (\(x, y) -> (length (takeWhile (< x) xs) * k + x, length (takeWhile (< y) ys) * k + y))

getGalaxies :: Int -> String -> [Coord]
getGalaxies n input = foldr (\(i, s) acc -> if s == '#' then (i `mod` n, i `div` n) : acc else acc) [] $ zip [0 .. length input] input

getExpandedX :: [String] -> [Int]
getExpandedX = getExpandedY . transpose

getExpandedY :: [String] -> [Int]
getExpandedY ls = foldr (\(i, l) acc -> if '#' `elem` l then acc else i : acc) [] $ zip [0 .. length ls] ls

expandUniverse :: [String] -> [String]
expandUniverse = foldl folder []
  where
    folder acc a = if (length . group) a == 1 then a : a : acc else a : acc

type Coord = (Int, Int)

main :: IO ()
main = do
  fileHandle <- openFile "input" ReadMode
  fileContents <- hGetContents fileHandle
  print (show $ solution1 fileContents)
  print (show $ solution2 fileContents)

  hClose fileHandle
