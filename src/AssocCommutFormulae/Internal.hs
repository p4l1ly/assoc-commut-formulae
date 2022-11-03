-- | Helper functions for AssocCommutFormulae

{-# LANGUAGE BlockArguments #-}
module AssocCommutFormulae.Internal where

-- | Remove nth element of the list
exceptAt :: Int -> [a] -> [a]
exceptAt _ [] = []
exceptAt 0 (_:xs) = xs
exceptAt n (x:xs) = x : exceptAt (n - 1) xs

-- | Given two natural numbers kMax and kSum, return all sorted sequences
-- (nonstrict descending) with elements lesser or equal to kMax sum of which
-- is kSum.
sortedSequences :: Int -> Int -> [[Int]]
sortedSequences 0 _ = [[]]
sortedSequences _ 0 = [[]]
sortedSequences kMax kSum = flip concatMap [kMax', kMax' - 1 .. 1] \n0 ->
  (n0 :) <$> sortedSequences n0 (kSum - n0)
  where
    kMax' = min kMax kSum

-- | Return elements at the given indices. Only the (nonstrictly) ascending
-- prefix of the indices is considered.
atSortedIxs :: [Int] -> [a] -> [a]
atSortedIxs = go 0
  where
    go _ _ [] = []
    go _ [] _ = []
    go consumed is@(i:is') xs@(x:xs')
      | consumed == i = x : go consumed is' xs
      | otherwise = go (consumed + 1) is xs'
