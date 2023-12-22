import Data.Array
import Data.Char (isDigit)
import Data.List
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe, isJust, mapMaybe)
import Debug.Trace (traceShow)
import System.IO

solution1 :: String -> Int
solution1 input = length $ filter (canBeDesintegrated settled) settled
  where
    settled = settle $ sort $ map parseLine $ lines input


solution2 :: String -> Int
solution2 input = sum $ map (getFallenAmount settled) [0..length settled - 1]
  where
    settled = settle $ sort $ map parseLine $ lines input

---------------------

getFallenAmount :: [(Coord, Coord)] -> Int -> Int
getFallenAmount cs i = amountOfUnmatchedItems zs zs'
    where zs = map (\(c1, _) -> z c1) (deleteAtIndex i cs)
          zs' = map (\(c1, _) -> z c1) (resettle cs i)

amountOfUnmatchedItems:: [Int] -> [Int] -> Int
amountOfUnmatchedItems zs1 zs2 = length $ filter id $ zipWith (/=) zs1 zs2


resettle:: [(Coord, Coord)] -> Int -> [(Coord, Coord)]
resettle s i = settle $ deleteAtIndex i s

deleteAtIndex :: Int -> [a] -> [a]
deleteAtIndex _ [] = []
deleteAtIndex 0 (x:xs) = xs
deleteAtIndex n (x:xs) = x : deleteAtIndex (n-1) xs

canBeDesintegrated :: [(Coord, Coord)] -> (Coord, Coord) -> Bool
canBeDesintegrated cs (c1, c2)
  | null myUps = True
  | otherwise = all stable myUps
  where
    myUps = getMyUps cs (c1, c2)
    stable (t1, t2) = length myDowns > 1
      where
        myDowns = getMyDowns cs (t1, t2)

getMyUps :: [(Coord, Coord)] -> (Coord, Coord) -> [(Coord, Coord)]
getMyUps cs (c1, c2) = filter (overlapsWith (c1, c2)) $ filter (\(c1', _) -> z c1' - z c2 == 1) cs


getMyDowns :: [(Coord, Coord)] -> (Coord, Coord) -> [(Coord, Coord)]
getMyDowns cs (t1, t2) = filter (overlapsWith (t1, t2)) $ filter (\(_, c2') -> z t1 - z c2' == 1) cs

overlapsWith :: (Coord, Coord) -> (Coord, Coord) -> Bool
overlapsWith a b = not $ null $ intersect (get2dCoords a) (get2dCoords b)

get2dCoords :: (Coord, Coord) -> [(Int, Int)]
get2dCoords (c1, c2) = [(x, y) | x <- range (x c1, x c2), y <- range (y c1, y c2)]

settle :: [(Coord, Coord)] -> [(Coord, Coord)]
settle cs = s cs Map.empty
  where
    s [cc] zs = [snd $ settleSingle zs cc]
    s (cc : cs) zs = cc' : s cs zs'
      where
        (zs', cc') = settleSingle zs cc

settleSingle :: Map.Map (Int, Int) Int -> (Coord, Coord) -> (Map.Map (Int, Int) Int, (Coord, Coord))
settleSingle zs (c1, c2) = (zs', nCoords)
  where
    xs1 = range (x c1, x c2)
    ys1 = range (y c1, y c2)
    allCoords = [(x, y) | x <- range (x c1, x c2), y <- range (y c1, y c2)]
    max = maximum $ map (\c -> fromMaybe 0 $ Map.lookup c zs) allCoords
    adjZ1 = min max (z c1) + 1
    adjZ2 = z c2 - (z c1 - adjZ1)
    nCoords = (Coord {x = x c1, y = y c1, z = adjZ1}, Coord {x = x c2, y = y c2, z = adjZ2})
    zs' = foldl (\acc x -> Map.insert x adjZ2 acc) zs allCoords

parseLine :: String -> (Coord, Coord)
parseLine line = (Coord {x = x1, y = y1, z = z1}, Coord {x = x2, y = y2, z = z2})
  where
    (x1 : y1 : z1 : x2 : y2 : z2 : _) = getNumbers line

getNumbers :: String -> [Int]
getNumbers = map read . filter (\a -> not (null a) && isDigit (head a)) . groupBy (\a b -> isDigit a == isDigit b)

data Coord = Coord {x :: Int, y :: Int, z :: Int} deriving (Show)

instance Eq Coord where
  (==) c1 c2 = x c1 == x c2 && y c1 == y c2 && z c1 == z c2

instance Ord Coord where
  compare (Coord x1 y1 z1) (Coord x2 y2 z2)
    | z1 /= z2 = compare z1 z2
    | x1 /= x2 = compare x1 x2
    | otherwise = compare y1 y2

main :: IO ()
main = do
  fileHandle <- openFile "example" ReadMode
  fileContents <- hGetContents fileHandle
  print (show $ solution1 fileContents)
  print (show $ solution2 fileContents)

  hClose fileHandle
