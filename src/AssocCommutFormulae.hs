-- | Generator of all formulae with associative and commutative binary operators.

{-# LANGUAGE BlockArguments #-}
module AssocCommutFormulae (generateAll, Formula(..)) where

import Data.List
import Data.Functor
import AssocCommutFormulae.Internal

-- ^ A formula with a single nullary symbol and multiple binary symbols.
data Formula
  = Leaf -- ^ The single nullary symbol of the formula
  | Op Int Formula Formula -- ^ Application of a binary operator (Op n)
  deriving (Eq, Ord)

instance Show Formula where
  show f1 = show0 f1

show0 :: Formula -> String
show0 Leaf = "."
show0 (Op i x1 x2) = maybeParen x1 ++ show i ++ maybeParen x2
  where
    maybeParen Leaf = "."
    maybeParen x@(Op i' _ _)
      | i' == i = show0 x
      | otherwise = '(' : show0 x ++ ")"

-- | Generate an infinite list of chunks of distinct formulae with the given
-- number of associative and commutative binary operators over a single
-- constant `Leaf`. By associative and commutative we mean that
-- a + (b * (c * d)) is not distinct from (c * (b * d)) + a.
generateAll :: Int -> [[Formula]]
generateAll opCount = [] : [Leaf] : step 2 (replicate opCount [])
  where
    step :: Int -> [[[Formula]]] -> [[Formula]]
    step level opTops = results : step (level + 1) opTops'
      where
        results = concat newOpTops
        opTops' = zipWith (\new olds -> olds ++ [new]) newOpTops opTops

        newOpTops :: [[Formula]]
        newOpTops =
          [0 .. opCount - 1] <&> \opIx ->
            map (foldr1 (Op opIx)) $
              step' ([Leaf] : (map concat $ transpose $ exceptAt opIx opTops))

        sortedSeqs :: [[Int]]
        sortedSeqs = sortedSequences (level - 1) level <&> map pred . reverse

        step' :: [[Formula]] -> [[Formula]]
        step' levels = flip concatMap sortedSeqs $ sequence . (`atSortedIxs` levels)
