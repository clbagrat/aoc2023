import Data.Char (isDigit, isSpace)
import Data.List (groupBy, tails)
import Data.Maybe (fromJust, isJust, mapMaybe)
import System.IO

solution1:: (Float, Float) -> String -> Int
solution1 bs = length . filter (insideBounds bs) . mapMaybe crossing . allPairs . map (parse2d . getNumbers) . lines

-- I Calculated solution for part 2 using wolfram alpha :D lol
-- https://www.wolframalpha.com/input?i=system+equation+calculator&assumption=%7B%22F%22%2C+%22SolveSystemOf3EquationsCalculator%22%2C+%22equation1%22%7D+-%3E%22Divide%5Bx+-+247058281022548+%2C89+-+v%5D+%3D+Divide%5By+-+192766389934431%2C450+-+b%5D%3DDivide%5Bz+-+81827832165583+%2C382+-+n%5D%22&assumption=%22FSelect%22+-%3E+%7B%7B%22SolveSystemOf3EquationsCalculator%22%7D%2C+%22dflt%22%7D&assumption=%7B%22F%22%2C+%22SolveSystemOf3EquationsCalculator%22%2C+%22equation2%22%7D+-%3E%22Divide%5Bx+-+238266575479152+%2C115+-+v%5D+%3D+Divide%5By+-+277109802279281%2C8+-+b%5D%3DDivide%5Bz+-+177024891330353%2C158+-+n%5D%22&assumption=%7B%22F%22%2C+%22SolveSystemOf3EquationsCalculator%22%2C+%22equation3%22%7D+-%3E%22Divide%5Bx+-+261045702761260+%2C80-+v%5D+%3D+Divide%5By+-+122483307789279%2C200+-+b%5D%3DDivide%5Bz+-+8764899903055%2C439-+n%5D%22

insideBounds :: (Float, Float) -> (Float, Float) -> Bool
insideBounds (bmin, bmax) (x, y) = x >= bmin && x <= bmax && y >= bmin && y <= bmax

crossing :: (Beam2d, Beam2d) -> Maybe (Float, Float)
crossing ((p0, n0), (p1, n1))
  | u < 0 || v < 0 = Nothing
  | m0 - m1 == 0 = Nothing
  | otherwise = Just (x, y)
  where
    dx = fst p1 - fst p0
    dy = snd p1 - snd p0
    det = (fst n1 * snd n0) - (snd n1 * fst n0)
    u = (dy * fst n1 - dx * snd n1) / det
    v = (dy * fst n0 - dx * snd n0) / det
    m0 = snd n0 / fst n0
    m1 = snd n1 / fst n1
    b0 = snd p0 - m0 * fst p0
    b1 = snd p1 - m1 * fst p1
    x = (b1 - b0) / (m0 - m1)
    y = m0 * x + b0

allPairs :: (Eq a) => [a] -> [(a, a)]
allPairs xs = [(x, y) | (x : rest) <- tails xs, y <- rest]

parse2d :: [Float] -> ((Float, Float), (Float, Float))
parse2d (x : y : _ : vx : vy : _) = ((x, y), norm (vx, vy))

norm :: (Float, Float) -> (Float, Float)
norm (x, y) = (x / magnitude, y / magnitude)
  where
    magnitude = sqrt (x * x + y * y)

getNumbers :: String -> [Float]
getNumbers = map (read . (\a -> if last a == ',' then init a else a)) . filter (`notElem` ["  ", " ", "@"]) . groupBy (\a b -> (not . isSpace) a == (not . isSpace) b)

type Beam2d = ((Float, Float), (Float, Float))

example :: IO ()
example = do
  fileHandle <- openFile "example" ReadMode
  fileContents <- hGetContents fileHandle
  print (show $ solution1 (7, 27) fileContents)

  hClose fileHandle

main :: IO ()
main = do
  fileHandle <- openFile "input" ReadMode
  fileContents <- hGetContents fileHandle
  print (show $ solution1 (200000000000000, 400000000000000) fileContents)

  hClose fileHandle
