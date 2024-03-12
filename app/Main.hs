module Main (main) where

import System.Directory (doesFileExist)
import System.Environment (getArgs)

import Hurtle.Types
import Hurtle.Parser
import Hurtle.Viewer

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> runFile fname
    _       -> putStrLn "Unexpected number of arguments, expected 1 argument:\n  - the file name of the hogo program"


runFile :: String -> IO ()
runFile fname = do
  fileExists <- doesFileExist fname
  if fileExists then do
    content <- readFile fname
    case parseHogoFile fname content of
      Left err      -> putStrLn err
      Right program -> renderHogo program
  else
    putStrLn $ "File " <> fname <> " not found"
