module Main where

import Data.List (sort)

main :: IO ()
main = do
  ls <- lines <$> readFile "input.txt"
  let numbers = read <$> ls
  let sortedNumbers = sort numbers
  print $ uncurry (*) <$> sum2020 sortedNumbers

-- The list must be sorted
sum2020 :: [Int] -> Maybe (Int, Int)
sum2020 ascending =
  let descending = reverse ascending
   in f ascending descending
  where
    f (a : as) (b : bs) = case compare (a + b) 2020 of
      GT -> f (a : as) bs
      EQ -> Just (a, b)
      LT -> f as (b : bs)
    f _ _ = Nothing
