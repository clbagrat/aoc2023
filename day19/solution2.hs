import Data.Char (isDigit)
import Data.List (find, groupBy)
import Data.Map qualified as Map
import Data.Maybe (fromJust, isJust)
import Debug.Trace (traceShow)
import System.IO

solution2 input = go workflows "in" Part {x = (1, 4000), m = (1, 4000), a = (1, 4000), s = (1, 4000)}
  where
    (workflowSource : _) = splitByTwoNewLines input
    workflows = extractWorkflows workflowSource

-----------------------------------------------------

go :: Map.Map String [Rule] -> String -> Part -> Int
go rs "A" p = traceShow p $ score p
go rs "R" p = 0
go rs c p = sum $ map (uncurry (go rs)) $ getNexts rules p
  where
    rules = fromJust $ Map.lookup c rs

--                          Workflow name
getNexts :: [Rule] -> Part -> [(String, Part)]
getNexts (r : rs) p
  | isJust ts && isJust fs = (next r, fromJust ts) : getNexts rs (fromJust fs)
  | isJust ts = [(next r, fromJust ts)]
  | isJust fs = getNexts rs (fromJust ts)
  where
    (ts, fs) =  eval p r

eval :: Part -> Rule -> (Maybe Part, Maybe Part)
eval p r
  | f r = (Just p, Nothing)
  | op r == '<' && lo > val r = (Nothing, Just p)
  | op r == '<' && hi < val r = (Just p, Nothing)
  | op r == '<' = (Just $ setter (lo, val r - 1), Just $ setter (val r, hi))
  | op r == '>' && hi < val r = (Nothing, Just p)
  | op r == '>' && lo > val r = (Just p, Nothing)
  | op r == '>' = (Just $ setter (val r + 1, hi), Just $ setter (lo, val r))
  where
    (lo, hi) = getPartVal (t r) p
    setter = setPartVal (t r) p

setPartVal :: Char -> Part -> (Int, Int) -> Part
setPartVal 'x' p v = Part {x=v, m=m p, a=a p, s=s p}
setPartVal 'm' p v = Part {x=x p, m=v, a=a p, s=s p}
setPartVal 'a' p v = Part {x=x p, m=m p, a=v, s=s p}
setPartVal 's' p v = Part {x=x p, m=m p, a=a p, s=v}

getPartVal :: Char -> Part -> (Int, Int)
getPartVal 'x' p = x p
getPartVal 'm' p = m p
getPartVal 'a' p = a p
getPartVal 's' p = s p

score :: Part -> Int
score part = (x2 - x1 + 1) * (m2 - m1 + 1) * (a2 - a1 + 1) * (s2 - s1 + 1)
  where
    (x1, x2) = x part
    (m1, m2) = m part
    (a1, a2) = a part
    (s1, s2) = s part

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

data Part = Part {x :: (Int, Int), m :: (Int, Int), a :: (Int, Int), s :: (Int, Int)} deriving (Show)

data Rule = Rule {f :: Bool, t :: Char, op :: Char, val :: Int, next :: String} deriving (Show)

main :: IO ()
main = do
  fileHandle <- openFile "input" ReadMode
  fileContents <- hGetContents fileHandle
  print (show $ solution2 fileContents)

  hClose fileHandle
