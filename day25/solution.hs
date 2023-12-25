{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

import Data.Char (isSpace)
import Data.List (groupBy, nub, sortBy, tails)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Internal.Debug (showTree)
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace (traceShow)
import System.IO

solution1 :: String -> Int
solution1 i = sizeLeft * sizeRight
  where
    edges = concatMap parse (lines i)
    allNodes = Set.toList $ foldl (\acc (e1, e2) -> Set.insert e2 $ Set.insert e1 acc) Set.empty edges
    graph = toGraph edges
    combos = [(a, b) | a <- allNodes, b <- reverse allNodes, a /= b]
    combosToCheck = takeEvenlyDistributed 10000 combos
    (g', (l, r)) = last $ take 4 $ iterate (cut1 combosToCheck) (graph, ("", ""))
    sizeLeft = size g' l
    sizeRight = size g' r

cut1 :: [Edge] -> (Graph, Edge) -> (Graph, Edge)
cut1 ns (g, _) = (cut g popular, popular)
  where
    popular = findMostPopularEdge g ns

cut :: Graph -> Edge -> Graph
cut g (n1, n2) =
  Map.adjust (Set.delete n2) n1
    . Map.adjust (Set.delete n1) n2
    $ g

size :: Graph -> Node -> Int
size graph node = go [] [node]
  where
    go vs [] = length vs
    go vs q = go vs' q'
      where
        vs' = nub $ vs ++ q
        q' = concatMap (filter (`notElem` vs) . neighbours graph) q

neighbours :: Graph -> Node -> [Node]
neighbours g n = Set.elems (g Map.! n)

findMostPopularEdge :: Graph -> [(Node, Node)] -> Edge
findMostPopularEdge g es = getMostPopular $ concatMap (bfs' g) es

getMostPopular :: [Edge] -> Edge
getMostPopular = go Map.empty
  where
    go m [] = getMaxFromMap m
    go m ((e1, e2) : es)
      | Map.member (e1, e2) m = go (Map.adjust (+ 1) (e1, e2) m) es
      | Map.member (e2, e1) m = go (Map.adjust (+ 1) (e2, e1) m) es
      | otherwise = go (Map.insert (e1, e2) 1 m) es

getMaxFromMap :: Map Edge Int -> Edge
getMaxFromMap xs = head $ Map.keys $ Map.filter (== m) xs
  where
    m = maximum $ Map.elems xs

bfs' :: Graph -> (Node, Node) -> [Edge]
bfs' g (s, t) = toPairs $ fromMaybe [] $ findPath t [(s, [])] -- toPairs $ fromMaybe [] $ findPath t [] [s]
  where
    findPath t q
      | null q = Nothing
      | t == c = Just vs'
      | c `elem` vs = findPath t (tail q)
      | otherwise = findPath t q'
      where
        c = fst $ head q
        vs = snd $ head q
        vs' = vs ++ [c]
        q' = tail q ++ map (\a -> (a, vs')) (Set.toList (fromMaybe Set.empty (Map.lookup c g)))

toPairs :: [Node] -> [(Node, Node)]
toPairs [] = []
toPairs [_] = []
toPairs (x : y : xs) = (x, y) : toPairs (y : xs)

takeEvenlyDistributed :: Int -> [a] -> [a]
takeEvenlyDistributed n xs
  | n <= 0 = []
  | otherwise = [xs !! i | i <- takePositions n (length xs)]

takePositions :: Int -> Int -> [Int]
takePositions n len = nub [i * len `div` (n + 1) | i <- [1 .. n]]

parse :: String -> [Edge]
parse i = [(a, b) | a <- a's, b <- bs']
  where
    (a', bs) = break (== ':') i
    a's = [a']
    bs' = filter (/= " ") $ groupBy (\s s' -> (not . isSpace) s == (not . isSpace) s') $ tail bs

toGraph :: [Edge] -> Graph
toGraph es = go Map.empty es
  where
    go g [] = g
    go g ((e1, e2) : es) = go (addToGraph (addToGraph g e1 e2) e2 e1) es

addToGraph :: Graph -> Node -> Node -> Graph
addToGraph g n1 n2
  | Map.member n1 g = Map.adjust (Set.insert n2) n1 g
  | otherwise = Map.insert n1 (Set.singleton n2) g

type Edge = (String, String)

type Node = String

type Graph = Map Node (Set Node)

example :: IO ()
example = do
  fileHandle <- openFile "example" ReadMode
  fileContents <- hGetContents fileHandle
  print (show $ solution1 fileContents)

  hClose fileHandle

main :: IO ()
main = do
  fileHandle <- openFile "input" ReadMode
  fileContents <- hGetContents fileHandle
  print (show $ solution1 fileContents)

  hClose fileHandle
