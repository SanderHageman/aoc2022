module Main where

import           Data.Foldable (foldlM, foldrM)
import           Data.List
import           Debug.Trace
import           Text.Read (readMaybe)
import           Data.Functor

main :: IO ()
main = days >>= putStrLn

-- >>> days
-- "day1: 66616 and 199172\n"
days :: IO String
days = do
  let app = fst . foldl' f ("", 1)
      f (r, i) x = (r ++ "day" ++ show i ++ ": " ++ x ++ "\n", i + 1)
  sequence [day1,day2,day3] <&> app

-- >>> day1
-- "66616 and 199172"
day1 :: IO String
day1 = do
  input <- readFile "input/d1"
  let (p1, p2) = (head top3, sum top3)
      top3 = (take 3 . reverse . sort) elves
      elves = map sum (splitInput input)
      splitInput = foldr (split . readMaybe) [[]] . lines
        where
          split (Just i) (x:xs) = (i:x):xs
          split _ xs = []:xs
  return $ show p1 ++ " and " ++ show p2

-- >>> day2
-- "8933 and 11998"
day2 :: IO String
day2 = do
  input <- readFile "input/d2"
  let p1 = sum . map score  $ sets
      p2 = sum . map score2 $ sets
      sets = map (map head . words) $ lines input
      score  [l,r] = (flip (-) 87 . fromEnum) r + wi l r
      score2 [l,r] = score [l, case r of 'X' -> lose l
                                         'Y' -> toEnum (fromEnum l + 23)
                                         'Z' -> win l]
        where lose 'A' = 'Z'
              lose 'B' = 'X'
              lose 'C' = 'Y'
              win 'A' = 'Y'
              win 'B' = 'Z'
              win 'C' = 'X'
      wi 'A' 'Y' = 6
      wi 'A' 'Z' = 0
      wi 'B' 'Z' = 6
      wi 'B' 'X' = 0
      wi 'C' 'X' = 6
      wi 'C' 'Y' = 0
      wi _ _ = 3
  return $ show p1 ++ " and " ++ show p2

-- >>> day3
-- "8493 and 2552"
day3 :: IO String
day3 = do
  input <- readFile "input/d3"
  let i = lines input
      p1 = sum $ map (score . head) l
        where l = map (uncurry intersect . s) i
              s v = splitAt (length v `div` 2) v

      p2 = sum $ map (score . head . uncurry dblIntersect) $ group i
        where dblIntersect (a,b) = intersect a . intersect b
              group = map mktup . fst . foldr f ([[]],0)
                where f x (r,3)     = ([x]:r,1)
                      f x (r:rs,n)  = ((x:r):rs,n+1)
                      mktup [a,b,c] = ((a,b),c)

      score c = let v = fromEnum c
                in v - if v >= 97 then 96 else 38

  pure $ show p1 ++ " and " ++ show p2
