import Data.List (find, group, groupBy, sort, sortBy, elemIndex)
import Distribution.Compat.Prelude (fromMaybe, isDigit, isSpace)
import System.IO

solution1 :: String -> Int
solution1 = calcTotalWinnings . map snd . sortBy (winningHandOnTop groupCards getCardValue) . map toHandAndBid . lines

solution2 :: String -> Int
solution2 = calcTotalWinnings . map snd . sortBy (winningHandOnTop groupCardsWithJoker getCardValueWithJoker) . map toHandAndBid . lines

----------------------------------------------------------------------------------------

toHandAndBid :: String -> (String, Int)
toHandAndBid s = (fst breaked, read $ snd breaked)
  where breaked = break isSpace s

winningHandOnTop :: (String -> [String]) -> (Char -> Int) -> (String, Int) -> (String, Int) -> Ordering
winningHandOnTop groupF cardValue a b
  | length leftGroup > length rightGroup = LT
  | length leftGroup < length rightGroup = GT
  | length (head leftGroup) < length (head rightGroup) = LT
  | length (head leftGroup) > length (head rightGroup) = GT
  | otherwise = compareByChar cardValue leftHand rightHand
  where
    leftHand = fst a
    rightHand = fst b
    leftGroup = groupF leftHand
    rightGroup = groupF rightHand

getCardValue:: Char -> Int
getCardValue card =  fromMaybe 0 (elemIndex card "23456789TJQKA")

getCardValueWithJoker:: Char -> Int
getCardValueWithJoker card = fromMaybe 0 (elemIndex card "J23456789TQKA")

compareByChar:: (Char -> Int) ->  String -> String -> Ordering
compareByChar f [a] [b] = compare (f a) (f b)
compareByChar f (a:as) (b:bs)
      | f a > f b = GT
      | f a < f b = LT
      | otherwise = compareByChar f as bs

groupCards :: String -> [String]
groupCards = sortBy (flip (\a b -> compare (length a) (length b))) . group . sort

groupCardsWithJoker :: String -> [String]
groupCardsWithJoker hand = fillWithJokers jokersCount grouped
  where noJokerCard = filter (/= 'J') hand
        jokersCount = length hand - length noJokerCard
        grouped = sortBy (flip (\a b -> compare (length a) (length b))) $ group $ sort  noJokerCard

fillWithJokers :: Int -> [String] -> [String]
fillWithJokers n [] = [replicate n 'J']
fillWithJokers 0 xs = xs
fillWithJokers n (x: xs) =  (x ++ replicate n 'J' ):  xs

calcTotalWinnings :: [Int] -> Int
calcTotalWinnings bids = sum $ zipWith (*) [1 .. length bids] bids

g :: [Char] -> [[Char]]
g = group

main :: IO ()
main = do
  fileHandle <- openFile "input" ReadMode
  fileContents <- hGetContents fileHandle
  print (show $ solution1 fileContents)
  print (show $ solution2 fileContents)

  hClose fileHandle
