module Main where
import Debug.Trace
import Data.List

main :: IO ()
main = do
  day1

day1 :: IO ()
day1 = do
  input <- readFile "input/d1"
  let
      p2 = (sum . take 3 . reverse . sort) elves
      p1 = maximum elves
      elves = map (sum . map read . words) (splitDbl input)
      splitDbl ('\n':'\n':is) = [] : splitDbl is
      splitDbl (i:is) = let (x:xs) = splitDbl is
                            a = i : x in a:xs
      splitDbl [] = [[]]
  print p1
  print p2
