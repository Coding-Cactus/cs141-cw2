module Main (main) where

import System.Directory (doesFileExist)
import System.Environment (getArgs)

import Hurtle.Parser (parseHogoFile)
import Hurtle.Viewer (renderHogoAnimation)

{-
  Run the hogo file passed via a command line argument. If there are issues with the provided argument(s), appropriate
  messages are displayed.
-}
main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> runFile fname
    _       -> putStrLn "Unexpected number of arguments, expected 1 argument:\n  - the file name of the hogo program"

{-
  Displays the animation for a given hogo file.

  If the file is not found, an appropriate message is displayed.
  If the hogo file is invalid, the parse error is displayed.
  Otherwise, the beautiful animation is ran.
-}
runFile :: String -> IO ()
runFile fname = do
  fileExists <- doesFileExist fname
  if fileExists then do
    content <- readFile fname
    case parseHogoFile fname content of
      Left err      -> putStrLn err
      Right program -> renderHogoAnimation program
  else
    putStrLn $ "File " <> fname <> " not found"
