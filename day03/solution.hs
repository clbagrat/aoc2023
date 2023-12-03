import System.IO;
import Data.List;
import Data.Char;
import Data.Maybe (fromMaybe)

solution1::  String -> Int
solution1 input = sum $ getNumbersCloseToSymbols checker lineLen input numbers
  where lineLen = length (takeWhile (/= '\n') input) + 1
        numbers = getNumbers input
        checker c = isDigit c || c == '.'

solution2:: String -> Int
solution2 input = sum $ map product twoCogsNumbers
  where lineLen = length (takeWhile (/= '\n') input) + 1
        numbers = getNumbers input
        checker c = c /= '*'
        numbersCloseToCog = filter (any (isCloseToSymbol checker lineLen input)) numbers
        getCogCoords = getCoords checker lineLen input
        numbersByCogs = groupBy (\a b -> getCogCoords a == getCogCoords b ) $ sortBy (\a b -> compare (getCogCoords a) (getCogCoords b)) numbersCloseToCog
        numbersByCogsParsed = map (map (read . map snd)) numbersByCogs :: [[Int]]
        twoCogsNumbers = filter (\a -> length a == 2)  numbersByCogsParsed


----------------------------------------

getCoords :: (Char -> Bool) -> Int -> String -> [(Int, Char)] -> Int
getCoords checker lineLen input =  fromMaybe 0 . find (\a -> not $ checker $ input!!a) . generateNeighborIndexes lineLen (length input) . fst . fromMaybe (0, 'p') . find (isCloseToSymbol checker lineLen input)

getNumbers ::  String -> [[(Int, Char)]]
getNumbers =  filter (any (isDigit . snd))
              . groupBy (\ a b -> isDigit (snd a) == isDigit (snd b))
              . zip [0..]

generateNeighborIndexes :: Int -> Int -> Int -> [Int]
generateNeighborIndexes lineLength inputLength index =
 map (\a -> fst a * lineLength + snd a) $
 filter (\a -> fst a >= 0 && snd a >= 0 && fst a < maxY && snd a < maxX)
 [(myY - 1 , myX - 1), (myY - 1, myX), (myY - 1, myX + 1),
  (myY, myX - 1), (myY, myX + 1),
  (myY + 1 , myX - 1), (myY + 1, myX), (myY + 1, myX + 1)]
  where myIndex = index
        myX = myIndex `mod` lineLength
        myY = myIndex `div` lineLength
        maxX = lineLength - 1
        maxY = inputLength `div` lineLength

isCloseToSymbol :: (Char -> Bool) -> Int -> String -> (Int, Char) -> Bool
isCloseToSymbol checker lineLen input = any (\a -> not $ checker $ input!!a) . generateNeighborIndexes lineLen inputLength . fst
  where inputLength = length input

getNumbersCloseToSymbols :: (Char -> Bool) -> Int -> String -> [[(Int, Char)]] -> [Int]
getNumbersCloseToSymbols checker lineLen input = map (read . map snd) . filter (any (isCloseToSymbol checker lineLen input))

main :: IO ()
main = do
  fileHandle <- openFile "input" ReadMode
  fileContents <- hGetContents fileHandle
  print (show $ solution1 fileContents)
  print (show $ solution2 fileContents)

  hClose fileHandle
