{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
import Data.Char (isLetter)
import Data.Either (fromLeft, fromRight, isLeft, isRight)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe, isJust)
import Debug.Trace (trace, traceShow)
import System.IO

solution1 input = l * h
  where
    rawModules = Map.fromList $ map createModule $ filter (\(c : _) -> c == '%' || c == '&') $ lines input
    (_ : _ : rawStarts) = words $ head $ filter (\(c : _) -> isLetter c) $ lines input
    starts = map (\a -> ("", a, False)) $ map init (init rawStarts) ++ [last rawStarts]
    modules = fillModules rawModules (Map.keys rawModules)
    (_, (l, h)) = foldl fs (modules, (0, 0)) [1 .. 1000]
    fs (ms, (lo, hi)) _ = (ms', (lo + lo', hi + hi'))
      where
        (ms', (lo', hi')) = pressButton1 ms starts

-- listen, i'm not proud of it, but it works

solution2 input = foldl lcm 1 highs 
  where
    parsedModules = map createModule $ filter (\(c : _) -> c == '%' || c == '&') $ lines input
    rawModules = Map.fromList parsedModules
    (_ : _ : rawStarts) = words $ head $ filter (\(c : _) -> isLetter c) $ lines input
    starts = map (\a -> ("", a, False)) $ map init (init rawStarts) ++ [last rawStarts]
    modules = fillModules rawModules (Map.keys rawModules)
    iterator = iterate (pressButton2 starts) (modules, [])
    toRx = map fst $ filter (\(_, m) -> "rx" `elem` getOuts m) parsedModules
    toToRx = concatMap (\a -> map fst $ filter (\(_, m) -> a `elem` getOuts m) parsedModules) toRx
    highs = map (\a -> length $ takeWhile (\(_, hs) -> a `notElem` hs) iterator ) toToRx

pressButton2 :: [(String, String, Bool)] -> (Modules, [String]) -> (Modules, [String])
pressButton2 starts (ms, hs) = processPulses2 ms starts []

getOuts:: Module -> [String]
getOuts = either ffOut cOut

--                                                     high out modules
processPulses2 :: Modules -> [(String, String, Bool)] -> [String] -> (Modules, [String])
processPulses2 ms [] hs = (ms, hs)
processPulses2 ms ((from, to, isHi) : ps) hs
  | not hasM = processPulses2 ms ps hs
  | otherwise = processPulses2 (Map.insert to m' ms) (ps ++ ps') hs'
  where
    hasM = isJust (Map.lookup to ms)
    (m', ps') = processSinglePulse (fromJust $ Map.lookup to ms) from to isHi
    hs' = if isHi then from : hs else hs

pressButton1 :: Modules -> [(String, String, Bool)] -> (Modules, (Int, Int))
pressButton1 ms starts = processPulses1 ms starts (length starts + 1, 0)

processPulses1 :: Modules -> [(String, String, Bool)] -> (Int, Int) -> (Modules, (Int, Int))
processPulses1 ms [] (lo, hi) = (ms, (lo, hi))
processPulses1 ms ((from, to, isHi) : ps) (lo, hi)
  | not hasM = processPulses1 ms ps (lo, hi)
  | otherwise = processPulses1 (Map.insert to m' ms) (ps ++ ps') (lo + lo', hi + hi')
  where
    hasM = isJust (Map.lookup to ms)
    (m', ps') = processSinglePulse (fromJust $ Map.lookup to ms) from to isHi
    lo' = length $ filter (\(_, _, isHi) -> not isHi) ps'
    hi' = length $ filter (\(_, _, isHi) -> isHi) ps'

--                               from       to      isHi
processSinglePulse :: Module -> String -> String -> Bool -> (Module, [(String, String, Bool)])
processSinglePulse m a t isHi = either processFlipFlop processConjunction m
  where
    processFlipFlop f
      | isHi = (m, [])
      | otherwise = (Left $ FlipFlop {isOn = not (isOn f), ffOut = ffOut f}, map (\s -> (t, s, not (isOn f))) $ ffOut f)
    processConjunction con = (Right $ Conjunction {mem = mem', cOut = cOut con}, map (\s -> (t, s, not allHi)) $ cOut con)
      where
        mem' = Map.insert a isHi (mem con)
        allHi = and (Map.elems mem')

fillModules :: Modules -> [String] -> Modules
fillModules m [] = m
fillModules m (k : ks)
  | isJust mod = fillModules (foldl (addToMem k) m (getCons m (fromJust mod))) ks
  | otherwise = fillModules m ks
  where
    mod = Map.lookup k m

--       module to mem      Conjunction module
addToMem :: String -> Modules -> String -> Modules
addToMem k ms t = Map.insert t (Right Conjunction {mem = Map.insert k False (mem con), cOut = cOut con}) ms
  where
    con = fromRight Conjunction {mem = Map.empty, cOut = []} $ fromJust (Map.lookup t ms)

getCons :: Modules -> Module -> [String]
getCons ms m
  | isLeft m = filter isCon $ ffOut (fromLeft FlipFlop {isOn = False, ffOut = []} m)
  | otherwise = filter isCon $ cOut $ fromRight Conjunction {mem = Map.empty, cOut = []} m
  where
    isCon k = isJust m' && isRight (fromJust m')
      where
        m' = Map.lookup k ms

createModule :: String -> (String, Module)
createModule line
  | t == '%' = (name, Left $ FlipFlop {isOn = False, ffOut = outs})
  | t == '&' = (name, Right $ Conjunction {mem = Map.empty, cOut = outs})
  where
    ((t : name) : _ : os) = words line
    outs = map init (init os) ++ [last os]

data FlipFlop = FlipFlop {isOn :: Bool, ffOut :: [String]} deriving (Show)

data Conjunction = Conjunction {mem :: Map.Map String Bool, cOut :: [String]} deriving (Show)

type Module = Either FlipFlop Conjunction

type Modules = Map.Map String Module

main :: IO ()
main = do
  fileHandle <- openFile "input" ReadMode
  fileContents <- hGetContents fileHandle
  print (show $ solution1 fileContents)
  print (show $ solution2 fileContents)

  hClose fileHandle
