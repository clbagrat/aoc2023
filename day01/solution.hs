import System.IO;
import Data.Char (isNumber, digitToInt)
import Text.Read (Lexeme(String))


words2numbers:: String -> String
words2numbers ('o':'n':'e':xs) = '1': words2numbers ('n' : 'e': xs)
words2numbers ('t':'w':'o':xs) = '2': words2numbers ('w' : 'o': xs)
words2numbers ('t':'h':'r':'e':'e':xs) = '3':words2numbers ('h':'r':'e':'e':xs)
words2numbers ('f':'o':'u':'r':xs) = '4':words2numbers ('o':'u':'r':xs)
words2numbers ('f':'i':'v':'e':xs) = '5':words2numbers ('i':'v':'e':xs)
words2numbers ('s':'i':'x':xs) = '6':words2numbers ('i':'x': xs)
words2numbers ('s':'e':'v':'e':'n':xs) = '7':words2numbers ('e':'v':'e':'n':xs)
words2numbers ('e':'i':'g':'h':'t':xs) = '8':words2numbers ('i':'g':'h':'t':xs)
words2numbers ('n':'i':'n':'e':xs) = '9':words2numbers ('i':'n':'e':xs)
words2numbers (x:xs) = x : words2numbers xs
words2numbers "" = ""

solveOneLine:: String -> Int
solveOneLine line = read [head nums, last nums] :: Int
                    where nums = filter isNumber line

-- solution :: String -> String
solution input = sum (map solveOneLine $ words input)

solution2 input = sum $ map (solveOneLine . words2numbers) (words input);


main :: IO ()
main = do
  fileHandle <- openFile "input" ReadMode
  fileContents <- hGetContents fileHandle
  putStrLn (show(solution fileContents) :: String)
  putStrLn (show(solution2 fileContents) :: String)
  hClose fileHandle

