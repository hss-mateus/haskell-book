module Main where

import Control.Monad (forever, when)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

-- Type synom for better readability

newtype WordList =
  WordList [String]

-- Word's size bounds

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

-- Read words from the dictionary file

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

-- Filter words based in the previous bounds

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where gameLength w =
          let l = length w
          in l >= minWordLength
             && l < maxWordLength

-- Get a random word from a word list

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

-- Get a random word from the gameWords value

randomWord' :: IO String
randomWord' = gameWords >>= randomWord


{- Puzzle's data

String: the word to guess
[Maybe Char]: the characters filled
[Char]: the letters guessed

-}

data Puzzle =
  Puzzle String [Maybe Char] [Char]

-- Instance of Show class for print the puzzle's data

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    intersperse ' ' $ fmap renderPuzzleChar discovered
    ++ " Guessed so far: " ++ guessed

-- Generate a new puzzle based on a word

freshPuzzle :: String -> Puzzle
freshPuzzle answer =
  Puzzle answer guesses []
  where guesses = map (const Nothing) answer

-- Check if a letter is present in the puzzle

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle answer _ _) guess =
  guess `elem` answer

-- Check if that letter have already guessed

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) guess =
  guess `elem` guessed

-- Turn a not guessed letter in a underscore

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just letter) = letter
renderPuzzleChar _ = '_'

-- Fill the correct answer into the correct guesses list

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
          then Just wordChar
          else guessChar
        newFilledInSoFar =
          zipWith (zipper c) word filledInSoFar

-- Returns a new puzzle based on the guess

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess
       , alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that\
               \ character, pick\
               \ something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the\
               \ word, filling in the word\
               \ accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This caracter wasn't in\
               \ the word, try again."
      return (fillInCharacter puzzle guess)

-- Stops the game after a number of guesses

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  when (length wrongGuesses > 5) $
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
         where wrongGuesses = filter isWrong guessed
               isWrong letter = letter `notElem` wordToGuess

-- Check if the player win

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  when (all isJust filledInSoFar) $
    do putStrLn "You win!"
       exitSuccess

-- A infinite loop that runs the game

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single character"

-- Generates a puzzle and runs the game

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
