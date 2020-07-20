module Main where

import Control.Monad (forever, when)
import Morse (stringToMorse, morseToChar)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (isEOF)

convertToMorse :: IO ()
convertToMorse = do
  line <- getLine
  convertLine line

  where
    convertLine line = do
      let morse = stringToMorse line
      case morse of
        (Just str) -> putStrLn $ unwords str
        Nothing -> do
          putStrLn $ "ERROR: " ++ line
          exitFailure

convertFromMorse :: IO ()
convertFromMorse = do
  line <- getLine
  convertLine line

  where
    convertLine line = do
      let decoded :: Maybe String
          decoded = traverse morseToChar (words line)
      case decoded of
        (Just s) -> putStrLn s
        Nothing -> do
          putStrLn $ "ERROR: " ++ line
          exitFailure

runLoop :: IO () -> IO ()
runLoop f = forever $ do
  weAreDone <- isEOF
  when weAreDone exitSuccess
 
  f

main :: IO ()
main = do
  mode <- getArgs
  case mode of
    ["from"] -> runLoop convertFromMorse
    ["to"] -> runLoop convertToMorse
    _ -> argError

  where argError = do
          putStrLn "Please specify a mode\n\
               \Available modes: from, to"
          exitFailure
