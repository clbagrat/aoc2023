import Data.List
import System.IO
import Data.Maybe (fromMaybe)

solution1 :: String -> Int
solution1 = sum . map (calcScore . findBestMirror isP . lines) . splitByTwoNewLines

solution2 :: String -> Int
solution2 = sum . map (calcScore . findBestMirror isP2 . lines) . splitByTwoNewLines

------------------------------------

calcScore :: (Int, Int) -> Int
calcScore (v, h) = v + h * 100

findBestMirror :: ([String] -> Int -> Bool) -> [String] -> (Int, Int)
findBestMirror f input = (v, h)
  where
    v = gmc f $ transpose $ reverse input
    h = gmc f input

gmc:: ([String] -> Int -> Bool) -> [String] -> Int
gmc f i = fromMaybe 0 $ find (f i) [1.. length i - 1]

isP :: [String] -> Int -> Bool
isP i x = take (length r) m == take (length m) r
      where m = slice x (length i) i
            r = reverse (slice 0 x  i)

isP2 :: [String] -> Int -> Bool
isP2 i x = length (filter (uncurry (/=)) $ zip tm tr) == 1
      where m = slice x (length i) i
            r = reverse (slice 0 x  i)
            tm = concat $ take (length r) m
            tr = concat $ take (length m) r

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from) (drop from xs)


splitByTwoNewLines :: String -> [String]
splitByTwoNewLines text = foldr build [[]] (init text)
  where
    build c (s : ls)
      | null s || head s /= '\n' || c /= '\n' = (c : s) : ls
      | otherwise = [] : tail s : ls

main :: IO ()
main = do
  fileHandle <- openFile "input" ReadMode
  fileContents <- hGetContents fileHandle
  print (show $ solution1 fileContents)
  print (show $ solution2 fileContents)

  hClose fileHandle
