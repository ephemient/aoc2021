{-|
Module:         Day6
Description:    Generates lookup tables for "Day6"
-}
{-# LANGUAGE TemplateHaskell, TypeApplications #-}
module Day6Meta (mkLUT) where

import Data.Array.Unboxed (IArray, UArray, (!), accumArray, bounds, elems, listArray, range)
import Data.Semigroup (stimes)
import Language.Haskell.TH.Syntax (lift)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

newtype Matrix a e = Matrix (a (Int, Int) e)

instance (IArray a e, Num e) => Semigroup (Matrix a e) where
    Matrix x <> Matrix y = Matrix $ listArray ((i0, j0), (i1, j1))
      [ sum $ zipWith (*) [x ! (i, k0) | k0 <- range (k00, k01)] [y ! (k1, j) | k1 <- range (k10, k11)]
      | (i, j) <- range ((i0, j0), (i1, j1))
      ] where
        ((i0, k00), (i1, k01)) = bounds x
        ((k10, j0), (k11, j1)) = bounds y

flatten :: (IArray a e, Num e) => Matrix a e -> a Int e
flatten (Matrix arr) = listArray (j0, j1)
    [sum [arr ! (i, j) | i <- range (i0, i1)] | j <- range (j0, j1)] where
    ((i0, j0), (i1, j1)) = bounds arr

step :: Matrix UArray Int
step = Matrix $ accumArray (const id) 0 ((0, 0), (8, 8)) $
    ((6, 0), 1) : ((8, 0), 1) : [((i, i + 1), 1) | i <- [0..7]]

mkLUT :: QuasiQuoter
mkLUT = QuasiQuoter
  { quoteExp = \times ->
        let lut = flatten $ stimes (read @Int times) step
        in [| listArray $(lift $ bounds lut) $(lift $ elems lut) |]
  , quotePat = error "unsupported"
  , quoteType = error "unsupported"
  , quoteDec = error "unsupported"
  }
