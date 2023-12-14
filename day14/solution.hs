import Data.List
import Data.Map (fromList, insert, lookup, member)
import Data.Maybe (fromJust)
import System.IO

solution1 :: String -> Int
solution1 = getScore . tilt . lines

solution2 :: String -> Int
solution2 input = getScore $ it !! i
  where
    it = iterate fullCycle $ lines input
    (ls, le) = getLoopIndex it
    i = ((1000000000 - ls) `mod` (le - ls)) + ls

------------------------------------

getScore :: [String] -> Int
getScore i = sum $ zipWith (\a x -> length (filter (== 'O') a) * x) (reverse i) [1 .. length i]

tilt :: [String] -> [String]
tilt i = last . take (length i) $ iterate shift i

shift :: [String] -> [String]
shift [x] = [x]
shift (x : y : ys) = x' : shift (y' : ys)
  where
    (x', y') = doRocks (x, y)

doRocks :: (String, String) -> (String, String)
doRocks (a, b) = foldr ((\(a, b) (as, bs) -> (a : as, b : bs)) . d) ("", "") (zip a b)
  where
    d ('.', 'O') = ('O', '.')
    d a = a

rotateRight :: [String] -> [String]
rotateRight = transpose . reverse

fullCycle :: [String] -> [String]
fullCycle = rotateRight . tilt . rotateRight . tilt . rotateRight . tilt . rotateRight . tilt

getLoopIndex :: [[String]] -> (Int, Int)
getLoopIndex it = f it 1 (fromList [(["sd"], 0)])
  where
    f it i m
      | member it' m = (fromJust (Data.Map.lookup it' m), i)
      | otherwise = f it (i + 1) (Data.Map.insert (it !! i) i m)
      where
        it' = it !! i


main :: IO ()
main = do
  fileHandle <- openFile "input" ReadMode
  fileContents <- hGetContents fileHandle
  print (show $ solution1 fileContents)
  print (show $ solution2 fileContents)

  hClose fileHandle
