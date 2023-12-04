import Data.List (groupBy)
import Distribution.Compat.Prelude (isDigit)
import System.IO

solution1 :: String -> Int
solution1 = sum . map (doubleEach . length . extractMyWinningNumbers . splitCard) . lines

solution2 :: String -> Int
solution2 = sum . ticketPlayTimes 0 [] . map splitCard . lines

---------------------------------------------------------

ticketPlayTimes :: Int -> [Int] -> [([Int], [Int])] -> [Int]
ticketPlayTimes i [] xs = ticketPlayTimes i (createNOnes (length xs)) xs
ticketPlayTimes i timesList [] = timesList
ticketPlayTimes i timesList (x : xs) = ticketPlayTimes (i + 1) (increaseListPartByN (i + 1) (length $ extractMyWinningNumbers x) (timesList !! i) timesList) xs

increaseListPartByN :: Int -> Int -> Int -> [Int] -> [Int]
increaseListPartByN _ 0 _ list = list
increaseListPartByN fromIndex amount delta list = zipWith (\i a -> if i >= fromIndex && i < fromIndex + amount then a + delta else a) [0 .. (length list)] list

splitCard :: String -> ([Int], [Int])
splitCard = mapTuple parseNumbers . break (== '|') . tail . dropWhile (/= ':')

extractMyWinningNumbers :: ([Int], [Int]) -> [Int]
extractMyWinningNumbers tuple = filter (`elem` winningNumbers) myNumbers
  where
    myNumbers = snd tuple
    winningNumbers = fst tuple

createNOnes :: Int -> [Int]
createNOnes n = [1 | _ <- [1 .. n]]

doubleEach :: Int -> Int
doubleEach 0 = 0
doubleEach n = foldl (\acc _ -> acc * 2) 1 (createNOnes (n - 1))

parseNumbers :: String -> [Int]
parseNumbers = map read . filter (\a -> not (null a) && isDigit (head a)) . groupBy (\a b -> isDigit a == isDigit b)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

main :: IO ()
main = do
  fileHandle <- openFile "input" ReadMode
  fileContents <- hGetContents fileHandle
  print (show $ solution1 fileContents)
  print (show $ solution2 fileContents)

  hClose fileHandle
