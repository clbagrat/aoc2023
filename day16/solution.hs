import Data.Array
import Data.List (nub)
import Data.Maybe
import Data.Set (Set, empty, fromList, insert, member, toList)
import System.IO

solution1 :: String -> Int
solution1 input = getEnergizedCells $ beam c2c (0, 0) (1, 0) empty
  where
    a = listArray (0, length input - 1) input
    l = length (takeWhile (/= '\n') input) + 1
    c2c = coordToChar l a

solution2 :: String -> Int
solution2 input = maximum $ map (\(sc, d) -> getEnergizedCells $ beam c2c sc d empty) $ concat [top, bot, left, right]
  where
    a = listArray (0, length input - 1) input
    l = length (takeWhile (/= '\n') input) + 1
    c2c = coordToChar l a
    vl = length input `div` l - 1
    top = map (\a -> ((a, 0), (0, 1))) [0 .. l - 2]
    bot = map (\a -> ((a, vl), (0, -1))) [0 .. l - 2]
    left = map (\a -> ((0, a), (1, 0))) [0 .. vl]
    right = map (\a -> ((l - 2, a), (-1, 0))) [0 .. vl]

---------------------------------------------

coordToChar :: Int -> Array Int Char -> Coord -> Maybe Char
coordToChar l a c
  | i < 0 || i >= length a || fst c >= l || fst c < 0 = Nothing
  | otherwise = Just (a ! (fst c + snd c * l))
  where
    i = fst c + snd c * l

getEnergizedCells :: Set (Coord, Coord) -> Int
getEnergizedCells = length . nub . map fst . toList

beam :: (Coord -> Maybe Char) -> Coord -> Coord -> Set (Coord, Coord) -> Set (Coord, Coord)
beam c2c sc d en
  | isNothing ch || member (sc, d) en || ch == Just '\n' = en
  | ch == Just '|' && (x d /= 0) = beam c2c (x sc, y sc + 1) (0, 1) $ beam c2c (x sc, y sc - 1) (0, -1) $ insert (sc, d) en
  | ch == Just '-' && (y d /= 0) = beam c2c (x sc + 1, y sc) (1, 0) $ beam c2c (x sc - 1, y sc) (-1, 0) $ insert (sc, d) en
  | ch == Just '\\' && (y d == 0) = beam c2c (x sc + x rrd, y sc + y rrd) rrd $ insert (sc, d) en
  | ch == Just '/' && (y d == 0) = beam c2c (x sc + x rld, y sc + y rld) rld $ insert (sc, d) en
  | ch == Just '\\' && (y d /= 0) = beam c2c (x sc + x rld, y sc + y rld) rld $ insert (sc, d) en
  | ch == Just '/' && (y d /= 0) = beam c2c (x sc + x rrd, y sc + y rrd) rrd $ insert (sc, d) en
  | otherwise = beam c2c (x sc + x d, y sc + y d) d $ insert (sc, d) en
  where
    ch = c2c sc
    rrd = rr d
    rld = rl d

x :: Coord -> Int
x = fst

y :: Coord -> Int
y = snd

rr :: Coord -> Coord
rr (-1, 0) = (0, -1)
rr (0, -1) = (1, 0)
rr (1, 0) = (0, 1)
rr (0, 1) = (-1, 0)

rl :: Coord -> Coord
rl (-1, 0) = (0, 1)
rl (0, 1) = (1, 0)
rl (1, 0) = (0, -1)
rl (0, -1) = (-1, 0)

type Coord = (Int, Int)

main :: IO ()
main = do
  fileHandle <- openFile "input" ReadMode
  fileContents <- hGetContents fileHandle
  print (show $ solution1 fileContents)
  print (show $ solution2 fileContents)

  hClose fileHandle
