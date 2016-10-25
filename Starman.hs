import System.Random
import System.IO

check :: String -> String -> Char -> (Bool, String)
check word display c =
  ( c `elem` word
  , [ if x == c
      then c
      else y
    | (x, y) <- zip word display
    ])

turn :: String -> String -> Int -> IO ()
turn word display n = do
  if n == 0
    then putStrLn ("You lose" ++ "\n" ++ "The word is " ++ word)
    else if word == display
           then putStrLn "You win!"
           else mkguess word display n

mkguess :: String -> String -> Int -> IO ()
mkguess word display n = do
  putStrLn (display ++ "  " ++ take n (repeat '*'))
  putStr "  Enter your guess: "
  q <- getLine
  let (correct, display') = check word display (q !! 0)
  let n' =
        if correct
          then n
          else n - 1
  turn word display' n'

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

randomWord :: IO [Char]
randomWord = do
  wordList <- readLines "enable1.txt"
  let wordListLength = length wordList
  rand <- randomRIO (0, (wordListLength) - 1)
  return $ wordList !! rand

starman :: Int -> IO ()
starman n = do
  word <- randomWord
  turn word ['-' | x <- word] n

main = starman 5
