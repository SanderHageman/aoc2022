module ParseLetters (readDisplay) where

import           Data.List.Split (splitOn, chunksOf)
import           Data.Bifunctor
import           Data.List

readDisplay :: [String] -> IO String
readDisplay disp = do
  letters <- letterMap
  let input = map concat $ transpose $ map (map init . chunksOf 5) disp
      res = map f input
      f s = case lookup s letters of
        Nothing -> error $ "Unrecognized character: " ++ s
        Just c  -> c
  pure res

letterMap :: IO [(String, Char)]
letterMap = map (bimap concat head . fmap concat . splitAt 6 . lines)
  . splitOn "\n\n"
  <$> readFile "input/alphabet"
