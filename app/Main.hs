{-# OPTIONS -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import           Data.List hiding (group)
import           Text.Read (readMaybe)
import           Data.Functor
import Data.List.Split (splitOn)
import Data.Sequence ((><))
import qualified Data.Sequence as Seq
import Data.Char
import Data.Foldable (Foldable(toList))

main :: IO ()
main = days >>= putStrLn

-- >>> days
-- "day1: 66616 and 199172\nday2: 8933 and 11998\nday3: 8493 and 2552\nday4: 569 and 936\nday5: \"RLFNRTNFB\" and \"MHQTLJRLB\"\n"
days :: IO String
days = do
  let app = fst . foldl' f ("", 1)
      f (r, i) x = (r ++ "day" ++ show i ++ ": " ++ x ++ "\n", i + 1)
  sequence [day1,day2,day3,day4,day5] <&> app

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

-- >>> day4
-- "569 and 936"
day4 :: IO String
day4 = do
  input <- readFile "input/d4"

  let pt = counttrue . flip map pairs
      counttrue = length . filter id

      pairs :: [[[Int]]]
      pairs = map split $ lines input where
        split = map (map read . splitOn "-") . splitOn ","

      fulCont [l,r] = con l r || con r l where
        con [p,q][x,y] = p <= x && y <= q

      parCont [l,r] = con l r || con r l where
        con [p,q][x,_] = p <= x && q >= x

  pure $ show (pt fulCont) ++ " and " ++ show (pt parCont)

-- >>> day5
-- "\"RLFNRTNFB\" and \"MHQTLJRLB\""
day5 :: IO String
day5 = do
  input <- readFile "input/d5"

  let [rawCra, rawPrc] = splitOn "\n\n" input

      cra = Seq.fromList $ map clnRow $ mkRows rawCra
        where
          clnRow = Seq.fromList . filter (/= ' ')
          mkRows = filter (any isAlpha) . transpose . init . lines

      prc = map parseProc $ lines rawPrc
        where
          parseProc = (\[a, b, c] -> (a, b - 1, c - 1)) . getN
          getN = map read . filter (any isNumber) . words

      apply fun st (n, f, t) =
        let tk = fun $ Seq.take n $ Seq.index st f
        in Seq.adjust' (Seq.drop n) f $ Seq.adjust' (tk ><) t st

      p1 = gettops $ foldl' (apply Seq.reverse) cra prc
      p2 = gettops $ foldl' (apply id) cra prc
      gettops = toList . fmap (`Seq.index` 0)

  pure $ show p1 ++ " and " ++ show p2
