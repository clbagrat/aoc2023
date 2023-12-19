import Data.Char (isDigit)
import Data.List (find, groupBy)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Debug.Trace (traceShow)
import System.IO

solution1 input = sum $ map score $ filter (go workflows "in") parts
  where
    (workflowSource : partsSource : _) = splitByTwoNewLines input
    parts = extractParts partsSource
    workflows = extractWorkflows workflowSource

-----------------------------------------------------

score :: Part -> Int
score part = x part + m part + a part + s part

--                           curWorkflow
go :: Map.Map String [Rule] -> String -> Part -> Bool
go rs "A" p = True
go rs "R" p = False
go rs w p = go rs (getNext rules p) p
  where
    rules = fromJust $ Map.lookup w rs

getNext :: [Rule] -> Part -> String
getNext rs p = next (fromJust $ find (eval p) rs)

eval :: Part -> Rule -> Bool
eval p r
  | f r = True
  | op r == '<' = getPartVal (t r) p < val r
  | op r == '>' = getPartVal (t r) p > val r

getPartVal :: Char -> Part -> Int
getPartVal 'x' p = x p
getPartVal 'm' p = m p
getPartVal 'a' p = a p
getPartVal 's' p = s p

extractParts :: String -> [Part]
extractParts input = map (pack . getNumbers) $ lines input
  where
    pack (x : m : a : s : _) = Part {x, m, a, s}

extractWorkflows :: String -> Map.Map String [Rule]
extractWorkflows input = foldl pack Map.empty $ lines input
  where
    pack acc line = Map.insert name (parseRules line) acc
      where
        name = takeWhile (/= '{') line

parseRules :: String -> [Rule]
parseRules line = go (tail $ init $ dropWhile (/= '{') line)
  where
    go ls = case break (== ',') ls of
      (x, "") -> [packRule x]
      (x, xs) -> packRule x : go (tail xs)

packRule :: String -> Rule
packRule line
  | ':' `elem` line = Rule {f = False, t = head line, op = line !! 1, val = head (getNumbers line), next = tail $ dropWhile (/= ':') line}
  | otherwise = Rule {f = True, t = '.', op = '.', val = 0, next = line}

getNumbers :: String -> [Int]
getNumbers = map read . filter (\a -> not (null a) && isDigit (head a)) . groupBy (\a b -> isDigit a == isDigit b)

splitByTwoNewLines :: String -> [String]
splitByTwoNewLines text = foldr build [[]] (init text)
  where
    build c (s : ls)
      | null s || head s /= '\n' || c /= '\n' = (c : s) : ls
      | otherwise = [] : tail s : ls

data Part = Part {x :: Int, m :: Int, a :: Int, s :: Int} deriving (Show)

data Rule = Rule {f :: Bool, t :: Char, op :: Char, val :: Int, next :: String} deriving (Show)

main :: IO ()
main = do
  fileHandle <- openFile "input" ReadMode
  fileContents <- hGetContents fileHandle
  print (show $ solution1 fileContents)

  hClose fileHandle
