{-# LANGUAGE BlockArguments #-}
module Main (main) where

import AssocCommutFormulae
import AssocCommutFormulae.Internal
import Control.Monad
import System.Exit

assert :: Bool -> IO ()
assert = (`unless` exitFailure)

main :: IO ()
main = do
  assert $ show (Op 0 Leaf (Op 0 Leaf (Op 1 Leaf Leaf))) == ".0.0(.1.)"
  assert $
    sortedSequences 4 5
    ==
    [ [4, 1]
    , [3, 2]
    , [3, 1, 1]
    , [2, 2, 1]
    , [2, 1, 1, 1]
    , [1, 1, 1, 1, 1]
    ]
  assert $ atSortedIxs [0, 1, 3, 3, 6] "abcdefgh" == "abddg"
  assert $ atSortedIxs [0, 1, 3, 2, 6] "abcdefgh" == "abd"  -- take only sorted prefix
  assert $
    map (map show) (take 5 $ generateAll 2)
    ==
    [ []
    , ["."]
    , [".0.", ".1."]
    , [ ".0(.1.)"
      , ".0.0."
      , ".1(.0.)"
      , ".1.1."
      ]
    , [ ".0(.1(.0.))"
      , ".0(.1.1.)"
      , "(.1.)0(.1.)"
      , ".0.0(.1.)"
      , ".0.0.0."
      , ".1(.0(.1.))"
      , ".1(.0.0.)"
      , "(.0.)1(.0.)"
      , ".1.1(.0.)"
      , ".1.1.1."
      ]
    ]
