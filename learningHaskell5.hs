
-- file: main.hs
import System.Environment (getArgs)
import Data.Char (toUpper, isUpper)

-- read input file, apply function to input file, display result
interactWith function inputFile =
  do input <- readFile inputFile
     putStrLn (function input)

-- starting point of the program: call interactWith on "myFunction"
-- where "myFunction" (with type String -> String) must be replaced
-- with the name of the function to call on the input. 
main =
  do args <- getArgs
     case args of
       [input] -> interactWith remWords input
       _ -> putStrLn "error: exactly one argument needed"

-- Question 1
toUpperCase :: String -> String
toUpperCase [] = []
toUpperCase str = map toUpper str

-- Question 2
capitals :: String -> String
capitals str = unwords(filter(\x -> isUpper (head x)) (words str))

-- Question 3
remWords :: String -> String
remWords str = foldl (remWord) str (words((head . lines) str))

remWord :: String -> String -> String
remWord str rmvWord = unwords(filter(\x -> x /= rmvWord) (words str))