module Main where

import           ResistorBuilder    (find)
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let target = read (head args) :: Float
  let result = find target 0
  case result of
    Just resistors -> do
      print resistors
    Nothing -> putStrLn "No solution found"
