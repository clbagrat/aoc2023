import System.IO
import Data.Map.Strict (fromList, Map, lookup, foldMapWithKey, foldrWithKey)
import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.List (groupBy)
import Data.Char (isLetter)


solution1 :: String -> Int
solution1 input =  walk (pickRoute instructions rules) 0 "AAA"
  where (instructions, rules) = getInstructionsAndRules $ lines input

solution2 :: String -> Int
solution2 input =  foldl lcm 1 . map (walkWildcard (pickRoute instructions rules) 0) $ starts
  where (instructions, rules) = getInstructionsAndRules $ lines input
        starts = filter (\a -> last a == 'A') $ foldrWithKey (\k x ks -> k:ks) [] rules

-------------------------------------------------------------


type Name = String
type LRef = String
type RRef = String
type Rule = (Name, (LRef, RRef))

type Instruction = String

walk:: (Int -> Name -> Name) -> Int -> Name -> Int
walk f c n
  | n == "ZZZ" = c
  | otherwise = walk f (c + 1) (f c n)

walkWildcard:: (Int -> Name -> Name) -> Int -> Name -> Int
walkWildcard f c n
  | last n == 'Z' = c
  | otherwise = walkWildcard f (c + 1) (f c n)

pickRoute:: Instruction -> Map Name (LRef, RRef) -> Int -> Name -> Name
pickRoute i m c n = if isLeft then fst refs else snd refs
  where isLeft = i!!(c `mod` length i) == 'L'
        refs = fromJust $ Data.Map.Strict.lookup n m


getInstructionsAndRules :: [String] -> (Instruction, Map Name (LRef, RRef))
getInstructionsAndRules (i:_:xs)= (i, createMap $ Prelude.map parseRule xs);

parseRule:: String -> Rule
parseRule (n1:n2:n3:_:_:_:_:l1:l2:l3:_:_:r1:r2:r3:_) = ([n1, n2, n3], ([l1, l2, l3], [r1, r2, r3]))

createMap :: [Rule] -> Map Name (LRef, RRef)
createMap = fromList



main :: IO ()
main = do
  fileHandle <- openFile "input" ReadMode
  fileContents <- hGetContents fileHandle
  print (show $ solution1 fileContents)
  print (show $ solution2 fileContents)

  hClose fileHandle
