module Main where
import Debug.Trace
import Data.List
import Text.Read (readMaybe)

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
      p2 = sum top3
      p1 = head top3
      top3 = (take 3 . reverse . sort) elves
      elves = map sum (splitInput input)
      splitInput = foldr (split . readMaybe) [[]] . lines
        where split (Just i) (x:xs) = (i:x):xs
              split _ xs = []:xs
  return $ show p1 ++ " and " ++ show p2
