module Main where
import Debug.Trace
import Data.List

main :: IO ()
main = do
  d1 <- day1
  putStrLn $ "day1: " ++ d1

-- >>> day1
-- "66616 and 199172"

day1 :: IO String
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
  return $ show p1 ++ " and " ++ show p2
