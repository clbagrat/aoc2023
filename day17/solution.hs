import Data.List
import Data.Map qualified as Map
import Data.Maybe (fromJust, isJust)
import Data.Set qualified as Set
import Debug.Trace
import System.IO

solution1 :: String -> Maybe Int
solution1 input = search grid isAccept getNexts
  where
    grid = string2GridNumbers input
    isAccept grid (x, y, dx, dy, c) = last (Map.keys grid) == (x, y)
    getNexts grid cost (x, y, dx, dy, c) =
      map (\(mbCost, b) -> (cost + fromJust mbCost, b)) $
        filter (\(mb, _) -> isJust mb) $
          filter (\(_, (x', y', _, _, _)) -> x' /= x || y' /= y) $
            [ (Map.lookup (x + rrx, y + rry) grid, (x + rrx, y + rry, rrx, rry, 1)),
              (Map.lookup (x + rlx, y + rly) grid, (x + rlx, y + rly, rlx, rly, 1))
            ]
              ++ ([(Map.lookup (x + dx, y + dy) grid, (x + dx, y + dy, dx, dy, c + 1)) | c < 3])
      where
        (rrx, rry) = rr (dx, dy)
        (rlx, rly) = rl (dx, dy)


solution2 :: String -> Maybe Int
solution2 input = search grid isAccept getNexts
  where
    grid = string2GridNumbers input
    isAccept grid (x, y, dx, dy, c) = last (Map.keys grid) == (x, y) && c >= 4
    getNexts grid cost (x, y, dx, dy, c) =
      map (\(mbCost, b) -> (cost + fromJust mbCost, b)) $
        filter (\(mb, _) -> isJust mb) $
          filter (\(_, (x', y', _, _, _)) -> x' /= x || y' /= y) $
            ([(Map.lookup (x + rrx, y + rry) grid, (x + rrx, y + rry, rrx, rry, 1)) | c >= 4 || (x, y) == (0, 0)])
              ++ ([(Map.lookup (x + rlx, y + rly) grid, (x + rlx, y + rly, rlx, rly, 1)) | c >= 4 || (x, y) == (0, 0)])
              ++ ([(Map.lookup (x + dx, y + dy) grid, (x + dx, y + dy, dx, dy, c + 1)) | c < 10])
      where
        (rrx, rry) = rr (dx, dy)
        (rlx, rly) = rl (dx, dy)

---------------------------------------

search :: Grid -> (Grid -> State -> Bool) -> (Grid -> Int -> State -> [(Int, State)]) -> Maybe Int
search grid isAccept getNexts = go Set.empty [(0, (0, 0, 1, 0, 1)), (0, (0, 0, 0, 1, 1))]
  where
    go seen q = traceShow (head q) $ case length q of
      0 -> Nothing
      _
        | cur `Set.member` seen -> go seen q'
        | isAccept grid cur -> Just cost
        | otherwise -> go seen' q''
        where
          q' = tail q
          (cost, cur) = head q
          nxts = getNexts grid cost cur
          q'' = sort $ q' ++ nxts
          seen' = Set.insert cur seen

string2Grid :: (Char -> a) -> String -> Map.Map Coord a
string2Grid processor input =
  Map.fromList $
    concat $
      zipWith
        ( \line y ->
            zipWith (\ch x -> ((x, y), processor ch)) line [0 ..]
        )
        (lines input)
        [0 ..]

string2GridNumbers :: String -> Grid
string2GridNumbers = string2Grid (\a -> read (a : ""))

rr :: Coord -> Coord
rr (x, y) = (-y, x)

rl :: Coord -> Coord
rl (x, y) = (y, -x)

type Coord = (Int, Int)

type Grid = Map.Map Coord Int

type State = (Int, Int, Int, Int, Int)

-----------------------------------------------

main :: IO ()
main = do
  fileHandle <- openFile "example" ReadMode
  fileContents <- hGetContents fileHandle
  print (show $ solution1 fileContents)
  print (show $ solution2 fileContents)

  hClose fileHandle
