{-# OPTIONS -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TupleSections #-}

module Main(main) where

import           Data.List hiding (group)
import           Text.Read (readMaybe)
import           Data.Functor
import Data.List.Split (splitOn, chunksOf)
import qualified Data.Sequence as Seq
import Data.Char
import Data.Foldable (Foldable(toList))
import Data.Maybe
import Control.Monad.State
import qualified Data.Set as Set
import ParseLetters (readDisplay)
import Text.Show.Functions ()
import Data.Bifunctor
import qualified Data.Foldable as Seq

main :: IO ()
main = days >>= putStrLn

-- >>> days
-- "day1: 66616 and 199172\nday2: 8933 and 11998\nday3: 8493 and 2552\nday4: 569 and 936\nday5: \"RLFNRTNFB\" and \"MHQTLJRLB\"\nday6: 1356 and 2564\nday7: 1084134 and 6183184\nday8: 1870 and 517440\nday9: 6470 and 2658\nday10: 16480 and PLEFULPB\n"
days :: IO String
days = do
  let app = fst . foldl' f ("", 1)
      f (r, i) x = (r ++ "day" ++ show i ++ ": " ++ x ++ "\n", i + 1)
  sequence [day1,day2,day3,day4,day5,day6,day7,day8,day9,day10, day11] <&> app

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
          clnRow = filter (/= ' ')
          mkRows = filter (any isAlpha) . transpose . init . lines

      prc = mapMaybe parseMove $ lines rawPrc
        where
          parseMove s = case words s of
            ["move", n, "from", f, "to", t]
              -> (,,) <$> readMaybe n <*> readMaybe f <*> readMaybe t
            _ -> error $ "Invalid input: " ++ s

      apply fun st (n, f, t) =
        let tk = fun $ take n $ Seq.index st (f - 1)
        in Seq.adjust' (drop n) (f - 1)
         $ Seq.adjust' (tk ++)  (t - 1) st

      p1 = gettops $ foldl' (apply reverse) cra prc
      p2 = gettops $ foldl' (apply id) cra prc
      gettops = toList . fmap (!! 0)

  pure $ show p1 ++ " and " ++ show p2

-- >>> day6
-- "1356 and 2564"
day6 :: IO String
day6 = do
  input <- readFile "input/d6"
  let chk s@(_:xs) n c = if (length . nub . take c) s == c
                         then n + c
                         else chk xs (n + 1) c
      p1 = chk input 0 4
      p2 = chk input 0 14
  pure $ show p1 ++ " and " ++ show p2

-- >>> day7
-- "1084134 and 6183184"

data Fs = Dir String Int [Fs] | File String Int
  deriving(Show)

day7 :: IO String
day7 = do
  input <- map tail
            . filter ((=="ls"). head . head)
            . tail . map (map words . lines)
            . splitOn "$ " <$> readFile "input/d7"

  let fs = evalState (traceDfs "/") input
      p1 = go fs where
        go File {} = 0
        go (Dir _ i ds) = sum (map go ds) +
                          if i > 100_000 then 0 else i

      p2 = minimum $ go fs where
        free = 70_000_000 - sz fs
        needed = 30_000_000 - free
        go (Dir _ i ds) = if i >= needed
                          then i : concatMap go ds
                          else []
        go File {} = []

      sz (Dir _ s _) = s
      sz (File _ s) = s

      traceDfs :: String -> State [[[String]]] Fs
      traceDfs dirName = do
        folder <- extractHead
        let toFs ["dir", s] = traceDfs s
            toFs [n, s] = pure $ File s $ read n
        subFs <- mapM toFs folder
        let fsize = foldr ((+) . sz) 0 subFs
        pure $ Dir dirName fsize subFs

      extractHead :: State [[[String]]] [[String]]
      extractHead = get >>= (\(d:ds) -> put ds >> return d)

  pure $ show p1 ++ " and " ++ show p2

-- >>> day8
-- "1870 and 517440"

day8 :: IO String
day8 = do
  input <- map (map digitToInt) . lines <$> readFile "input/d8"
  let a = zipWith (zip3 [0 ..] . repeat) [0 ..] input

      p1 = length . filter id . concatMap (map hasSight) $ a
      hasSight (x, y, n) = foldr ((||) . getSight (x, y)) False dirs
        where
          getSight prev dir =
            let cur = dir +- prev
            in case getAt cur of
                 Just (_, _, n') -> n > n' && getSight cur dir
                 Nothing         -> True

      p2 = maximum $ concatMap (map scenicScore) a
      dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)]
      scenicScore (x, y, n) = foldr ((*) . getSight (x, y)) 1 dirs
        where
          getSight prev dir =
            let cur = dir +- prev
            in case getAt cur of
                 Just (_, _, n') -> 1
                   + if n' >= n then 0
                     else getSight cur dir
                 Nothing         -> 0

      len = length a
      ye = Set.fromList $ map Set.fromList a
      getAt (x, y) = if x >= len || x < 0 || y >= len || y < 0
                     then Nothing else Just (Set.elemAt x $ Set.elemAt y ye)
  pure $ show p1 ++ " and " ++ show p2

-- tuple stuff
(+-) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(+-) (l, r) (l', r') = (l + l', r + r')
(-+) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(-+) (l, r) (l', r') = (l - l', r - r')

-- >>> day9
-- "6470 and 2658"

day9 :: IO String
day9 = do
  let dir c = case c of
            "R" -> (0,1)
            "L" -> (0,-1)
            "U" -> (1,0)
            "D" -> (-1,0)
      toTok [d, sN] = let n = read sN
                      in replicate n $ dir d

  input <- concatMap (toTok . words) . lines <$> readFile "input/d9"

  let p1 = a 2
      p2 = a 10
      a n = length $ snd $ foldl' f (replicate n (0,0), Set.empty) input
      f (h:kns, s) m = let hNewPos = h +- m
                           (tNew, lst) = foldl' q ([hNewPos], hNewPos) kns
                           q (l, p) x = let new = follow p x in (l++[new], new)
                        in (tNew, Set.insert lst s)
      follow h t = let (dX, dY) = h -+ t
                       offset n | abs n == 2 = 1 * signum n
                                | abs dX + abs dY < 3 = 0
                                | otherwise = n
                    in t +- (offset dX, offset dY)

  pure $ show p1 ++ " and " ++ show p2

data Inst = NoOp | AddX Int
  deriving Show

-- >>>day10
-- "16480 and PLEFULPB"

day10 :: IO String
day10 = do
  let toTok ["addx", sN] = [NoOp, AddX $ read sN]
      toTok ["noop"]     = [NoOp]

  input <- concatMap (toTok . words) . lines <$> readFile "input/d10"

  let p1 = sum $ map (\x -> x * states !! x) [20, 60..220]
      states = error "Array starts at 1 👺" : scanl' apply 1 input

      apply v (AddX n) = v + n
      apply v _ = v

      annotedInstr = zip repRange40 input
      repRange40 = concat $ repeat [0..39]
      (dispString, _) = foldl' f ([], 1) annotedInstr
      f (d, x) (p, op) =
        let draw = abs (p-x) <= 1
            puts | draw = "#"
                 | otherwise = "."
        in (d ++ puts, apply x op)
      display = chunksOf 40 dispString

  p2 <- readDisplay display

  pure $ show p1 ++ " and " ++ p2


-- >>>day11
-- "50616 and 11309046332"

data Monkey = Monkey (Int -> Int) Int Int Int
  deriving Show

mkDiv :: Monkey -> Int
mkDiv (Monkey _ tst _ _) = tst

day11 :: IO String
day11 = do
  let toMonkey :: [String] -> ([Int], Monkey) -- forgive my sins
      toMonkey [ns, op, tst, tr, fs] = (items, Monkey opera divby tTrue tFlse) where
        items = map read $ splitOn ", " ns
        opera = let [o,r] = (drop 3 . words) op
                    os | o == "*"  = (*)
                       | otherwise = (+)
                in case readMaybe r of
                  Nothing -> \n -> os n n
                  Just n  -> os n
        divby = getEndN tst
        tTrue = getEndN tr
        tFlse = getEndN fs
        getEndN = read . last . words

  input <- map (toMonkey . map ((\[_,r] -> r) . splitOn ": ") . tail . lines)
          . splitOn "\n\n" <$> readFile "input/d11"

  let (startItems, monkeys) =
        bimap (Seq.fromList . zip (zip [0..] (repeat 0)))
        (Seq.fromList . zip [0..]) (unzip input)

      p1 = let [m1,m2] = monkeyBusiness $ monkeyRounds [0..19]   False in m1 * m2
      p2 = let [m1,m2] = monkeyBusiness $ monkeyRounds [0..9999] True  in m1 * m2

      monkeyBusiness = take 2 . reverse . sort . map (snd . fst) . Seq.toList

      monkeyRounds range isP2 = foldl' monkeyRound startItems range
        where
          monkeyRound r _ = foldl' f arg r
          f s ((idx, nInsp), is) = Seq.adjust' newSelf idx $ foldl' modi s inspItems
            where
              modi s' (i, n) = Seq.adjust' (second (++[n])) i s'
              newSelf _ = ((idx, nInsp + newInsp),[])
              is' = is ++ snd (Seq.index s idx)
              inspItems = map inspect is'
              newInsp = length is'
              (Monkey op tst tr fs) = snd $ Seq.index monkeys idx
              inspect x = (,newVal) $ if newVal `mod` tst == 0 then tr else fs
                where newVal | isP2      = op x `mod` p2Mod
                             | otherwise = op x `div` 3

          nMonkeys = length monkeys
          arg = Seq.fromList $ zip (zip [0..nMonkeys-1] (repeat 0)) (repeat [])
          p2Mod = product . map (mkDiv . snd) $ Seq.toList monkeys

  pure $ show p1 ++ " and " ++ show p2
