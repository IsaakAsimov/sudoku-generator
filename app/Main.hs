module Main where

import Data.List          (intersperse, intercalate)
import GeneratorFunctions (backtrack, remove, newStdGen)
import Shuffle            (shuffle)
import System.Environment (getArgs)


main :: IO ()
main = do
    let emptyBoard = replicate 9 (replicate 9 0)
    n           <- read . head    <$> getArgs
    (list, gen) <- shuffle [1..9] <$> newStdGen
    sdk         <- backtrack emptyBoard (0,0) 81 list <$> newStdGen
    printGrid $ remove (81 - n) sdk gen

printGrid :: [[Int]] -> IO ()
printGrid xs = mapM_ putStrLn $ line : intersperse line strl2 ++ [line]
  where
    strl2 = map (\x -> "| " ++ intercalate " | " x ++ " |") strl1
    strl1 = map (map (\x -> if x == 0 then " " else show x)) xs
    line  = "+---+---+---+---+---+---+---+---+---+"
