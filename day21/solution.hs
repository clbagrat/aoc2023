import Data.Array
import Data.List (elemIndex, findIndex, nub)
import Data.Maybe (fromJust, isJust)
import System.IO

solution1 input = dp c2c startCoord 64
  where
    l = length (takeWhile (/= '\n') input) + 1
    i2c = indexToCoord l
    inputArray = listArray (0, length input - 1) input
    c2c = coordToChar inputArray l
    startCoord = i2c $ fromJust (elemIndex 'S' input)

-- ALL CREDITS TO HYPER NEITRINO
solution2 input =
  odd * oddPoints
    + even * evenPoints
    + ct
    + cr
    + cb
    + cl
    + (w + 1) * (str + stl + sbr + sbl)
    + w * (ltr + ltl + lbr + lbl)
  where
    l = length (takeWhile (/= '\n') input) + 1
    i2c = indexToCoord l
    inputArray = listArray (0, length input - 1) input
    c2c = coordToChar inputArray l
    startCoord = i2c $ fromJust (elemIndex 'S' input)
    size = length $ lines input
    steps = 26501365
    w = steps `div` size - 1
    odd = ((w `div` 2) * 2 + 1) ^ 2
    even = (((w + 1) `div` 2) * 2) ^ 2
    oddPoints = dp c2c startCoord (size * 2 + 1)
    evenPoints = dp c2c startCoord (size * 2)
    ct = dp c2c (fst startCoord, size - 1) (size - 1)
    cr = dp c2c (0, snd startCoord) (size - 1)
    cb = dp c2c (fst startCoord, 0) (size - 1)
    cl = dp c2c (size - 1, snd startCoord) (size - 1)

    str = dp c2c (0, size - 1) (size `div` 2 - 1)
    stl = dp c2c (size - 1, size - 1) (size `div` 2 - 1)
    sbr = dp c2c (0, 0) (size `div` 2 - 1)
    sbl = dp c2c (size - 1, 0) (size `div` 2 - 1)

    ltr = dp c2c (0, size - 1) ((size * 3 `div` 2) - 1)
    ltl = dp c2c (size - 1, size - 1) ((size * 3 `div` 2) - 1)
    lbr = dp c2c (0, 0) ((size * 3 `div` 2) - 1)
    lbl = dp c2c (size - 1, 0) ((size * 3 `div` 2) - 1)

dp :: (Coord -> Maybe Char) -> Coord -> Int -> Int
dp c2c startCoord target = length $ d target
  where
    d t
      | t == 0 = [startCoord]
      | otherwise = nub (concatMap gfn (ds ! (t - 1)))

    ds = listArray bs [d t | t <- range bs]
    bs = (0, target)
    gfn = getFreeNeighbors c2c

getFreeNeighbors :: (Coord -> Maybe Char) -> Coord -> [Coord]
getFreeNeighbors c2c (x, y) = filter (\c -> isJust (c2c c) && fromJust (c2c c) /= '#') [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

indexToCoord :: Int -> Int -> Coord
indexToCoord l i = (i `mod` l, i `div` l)

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

type Coord = (Int, Int)

main :: IO ()
main = do
  fileHandle <- openFile "input" ReadMode
  fileContents <- hGetContents fileHandle
  print (show $ solution1 fileContents)
  print (show $ solution2 fileContents)

  hClose fileHandle
