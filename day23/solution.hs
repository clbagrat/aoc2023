{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use bimap" #-}
import Data.Array
import Data.List (elemIndex, find, findIndex)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Set (Set, empty, insert, member, notMember)
import Debug.Trace (traceShow)
import System.IO


solution1 input = walk c2c getNeighbors target empty (1, 0) 0
  where
    lns = lines input
    l = length lns
    target = (fromJust $ elemIndex '.' $ last lns, l - 1)
    arr = listArray (0, length input - 1) input
    c2c = coordToChar arr (l + 1)

solution2 input = walk2 c2c target empty (1, 0) (0, 1) 0
  where
    lns = lines input
    l = length lns
    target = (fromJust $ elemIndex '.' $ last lns, l - 1)
    arr = listArray (0, length input - 1) input
    c2c = coordToChar arr (l + 1)

--------------------------------------
--
--         coord2char              target     visited    current  direction
walk2 :: (Coord -> Maybe Char) -> Coord -> Set Coord -> Coord -> Coord -> Int -> Int
walk2 c2c t vs c d i
  | c == t = traceShow ("end" ++ show i) i
  | lc == t = traceShow ("END" ++ show i') i'
  | member c vs = traceShow ("seen " ++ show c)  0
  | c2c c `elem` map Just ['>', '<', 'v', '^'] = traceShow ("Meet ^"++show c++show nc) walk2 c2c t (insert c vs) nc d (i + 1)
  | c == lc = traceShow ("split" ++ show lc ++ show mds) $ maximum $ map(\c' -> walk2 c2c t (insert lc vs) c' (getDir c' lc) (i+1))  mds
  | nch == Just '#' = traceShow ("Meet #" ++ show lc ++ show sc) $ walk2 c2c t vs' sc (getDir sc lc)  i' 
  | nch `elem` map Just ['>', '<', 'v', '^']  = traceShow "NEXT ^" $ walk2 c2c t vs' nc d i'
  | otherwise = traceShow "OTHERWISE" 0
  where
    it = iterate (\(x, y) -> (x + fst d, y + snd d)) c
    skipped =  takeWhile (\c' -> c2c c' == Just '.') it
    lc = if null skipped then c else last skipped
    nc = (fst lc + fst d, snd lc + snd d)
    nch = c2c nc
    i' = i + length skipped
    nDirs = getInDir (filter (/= (-(fst d), -(snd d))) allDirs) lc
    vs' = foldl (flip insert) vs skipped
    sc = head $ filter (\c -> c2c c `notElem` map Just ['#', '\n']) nDirs
    mds = filter (`notMember` vs) $ filter (\c -> c2c c `notElem` map Just ['#', '\n']) nDirs


--         coord2char              getNeighbords       target     visited    current
walk :: (Coord -> Maybe Char) -> (Coord -> [Coord]) -> Coord -> Set Coord -> Coord -> Int -> Int
walk c2c gn t vs c i
  | c == t = i
  | member c vs = 0
  | null ns = 0
  | isNothing mCh = 0
  | ch `elem` ['>', '<', 'v', '^'] = walk c2c gn t vs' (getNext ch c) i + 1 -- get one in direction
  | otherwise = maximum $ map (\c' -> walk c2c gn t vs' c' (i + 1)) ns -- max of neighbors
  where
    ns = filter (\c' -> isJust (c2c c') && fromJust (c2c c') `notElem` ['#', '\n']) $ getNeighbors c
    vs' = insert c vs
    mCh = c2c c
    ch = fromJust mCh

allDirs = [up, left, down, right]

getInDir:: [Coord] -> Coord -> [Coord]
getInDir dirs (x,y) = map (\(x1, y1) -> (x + x1, y + y1)) dirs

getNeighbors :: Coord -> [Coord]
getNeighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

manDist :: Coord -> Coord -> Int
manDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

clamp :: Int -> Int -> Int -> Int
clamp mn mx val = max mn (min val mx)

getDir :: Coord -> Coord -> Coord
getDir (x1, y1) (x2, y2) =  (clamp (-1) 1 (x1 - x2), clamp (-1) 1 (y1 - y2))

getNext :: Char -> Coord -> Coord
getNext '<' (x, y) = (x - 1, y)
getNext '>' (x, y) = (x + 1, y)
getNext 'v' (x, y) = (x, y + 1)
getNext '^' (x, y) = (x, y - 1)

coordToIndex :: Int -> Coord -> Int
coordToIndex l c = snd c * l + fst c

coordToChar :: Array Int Char -> Int -> Coord -> Maybe Char
coordToChar arr l c
  | isValid = Just $ arr ! i
  | otherwise = Nothing
  where
    i = coordToIndex l c
    bs = bounds arr
    isValid = i >= fst bs && i <= snd bs

up = (0, -1)

down = (0, 1)

left = (-1, 0)

right = (1, 0)

type Coord = (Int, Int)

main :: IO ()
main = do
  fileHandle <- openFile "input" ReadMode
  fileContents <- hGetContents fileHandle
  print (show $ solution1 fileContents)
  print (show $ solution2 fileContents)

  hClose fileHandle
