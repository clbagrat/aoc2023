import Data.List (elemIndex, findIndex, groupBy, nub, sort)
import Data.Maybe (fromJust)
import System.IO

solution1 :: String -> Int
solution1 input = div (length circlePath) 2
  where
    lineLength = length (takeWhile (/= '\n') input) + 1
    i2c = indexToCoord lineLength
    c2i = coordToIndex lineLength
    circlePath = getCircledPath c2i input [] $ i2c $ findStartIndex input


solution2 :: String -> Int
solution2 input = length $ filter id $ map (isInsideCircle cbc circlePath) $ zipWith (\a i -> i2c i ) input [0..]
  where
    lineLength = length (takeWhile (/= '\n') input) + 1
    i2c = indexToCoord lineLength
    c2i = coordToIndex lineLength
    cbc = getCharByCoord c2i input
    circlePath = getCircledPath c2i input [] $ i2c $ findStartIndex input

------------------------------------------------

isInsideCircle :: (Coord -> Char) -> [Coord] -> Coord -> Bool
isInsideCircle cbc path (y, x) =  vert `mod` 2 == 1 && hor `mod` 2 == 1
  where vert = countVertCrossings cbc path (map (, x) [0..y])
        hor = countHorCrossings cbc path (map (y, ) [0..x])

countVertCrossings :: (Coord -> Char) -> [Coord] -> [Coord]  -> Int
countVertCrossings cbc path cs
  | last cs `elem` path = 0
  | otherwise = div (foldl foldVertWeight 0 $ map (replaceS path . cbc) (filter (`elem` path) cs) ) 2

countHorCrossings :: (Coord -> Char) -> [Coord] -> [Coord]  -> Int
countHorCrossings cbc path cs
  | last cs `elem` path = 0
  | otherwise = div (foldl foldHorWeight 0 $ map (replaceS path . cbc) (filter (`elem` path) cs) ) 2

replaceS :: [Coord] -> Char -> Char
replaceS cs 'S'
  | s == (1, 1) = 'F'
  | s == (-1, -1) = 'J'
  | s == (1, -1) = '7'
  | s == (-1, 1) = 'L'
  | fst a == fst b = '|'
  | snd a == snd b = '-'
  | otherwise = 'X'
  where a = head cs
        b = last (init cs)
        c = last cs
        d = (fst a - fst c, snd a - snd c)
        e = (fst b - fst c, snd b - snd c)
        s = (fst d + fst e, snd d + snd e)
replaceS _ l = l;


foldVertWeight :: Int -> Char -> Int
foldVertWeight acc '-' = acc + 2
foldVertWeight acc 'F' = acc + 1
foldVertWeight acc 'J' = acc + 1
foldVertWeight acc 'L' = acc - 1
foldVertWeight acc '7' = acc - 1
foldVertWeight acc _ = acc

foldHorWeight :: Int -> Char -> Int
foldHorWeight acc '|' = acc + 2
foldHorWeight acc 'F' = acc + 1
foldHorWeight acc 'J' = acc + 1
foldHorWeight acc 'L' = acc - 1
foldHorWeight acc '7' = acc - 1
foldHorWeight acc _ = acc

getCircledPath :: C2I -> String -> [Coord] -> Coord -> [Coord]
getCircledPath c2i input path coord
  | getCharByCoord c2i input coord == 'S' && not (null path) = path
  | otherwise = getCircledPath c2i input (coord : path) (getNextCoord c2i input path coord)

findStartIndex :: String -> Int
findStartIndex = fromJust . elemIndex 'S'

indexToCoord :: Int -> I2C
indexToCoord lineLength i = (div i lineLength, mod i lineLength)

coordToIndex :: Int -> C2I
coordToIndex lineLength c = fst c * lineLength + snd c

getCharByCoord :: C2I -> String -> Coord -> Char
getCharByCoord c2i input c = input !! c2i c

getNextCoord :: C2I -> String -> [Coord] -> Coord -> Coord
getNextCoord c2i input [] c = fst $ head $ filter (isConnected c2i input c) $ generateNeighborPipes c2i input c
getNextCoord c2i input path c = fst $ head $ filter (\a -> fst a /= head path) $ filter (isConnected c2i input c) $ generateNeighborPipes c2i input c

generateNeighborPipes :: C2I -> String -> Coord -> [Pipe]
generateNeighborPipes c2i input (y, x) = map (coordToPipe c2i input) $ filter (\i -> c2i i < length input) $ filter (\(y1, x1) -> x1 >= 0 && y1 >= 0) [(y, x + 1), (y - 1, x), (y, x - 1), (y + 1, x)]

coordToPipe :: C2I -> String -> Coord -> Pipe
coordToPipe c2i input c = (c, getOuts (getCharByCoord c2i input c))

getOuts :: Char -> [Coord]
getOuts '7' = [(0, -1), (1, 0)]
getOuts '|' = [(-1, 0), (1, 0)]
getOuts '-' = [(0, -1), (0, 1)]
getOuts 'L' = [(-1, 0), (0, 1)]
getOuts 'J' = [(0, -1), (-1, 0)]
getOuts 'F' = [(0, 1), (1, 0)]
getOuts 'S' = [(1, 0), (0, 1), (-1, 0), (0, -1)]
getOuts _ = [(0, 0)]

isConnected :: C2I -> String -> Coord -> Pipe -> Bool
isConnected c2i input c p = any (\c' -> (fst pCoord + fst c', snd pCoord + snd c') == c) pOuts && any (\c' -> (fst c + fst c', snd c + snd c') == pCoord) cOuts
  where
    pCoord = fst p
    pOuts = snd p
    (_, cOuts) = coordToPipe c2i input c

type Coord = (Int, Int)

type I2C = (Int -> Coord)

type C2I = (Coord -> Int)

type Pipe = (Coord, [Coord])

main :: IO ()
main = do
  fileHandle <- openFile "input" ReadMode
  fileContents <- hGetContents fileHandle
  print (show $ solution1 fileContents)
  print (show $ solution2 fileContents)

  hClose fileHandle
