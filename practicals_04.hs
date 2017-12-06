import Prelude ()
import MiniPrelude

-- (1)

descend :: Nat -> List Nat
descend 0 = []
descend n = n : descend (n - 1)

-- (a)
-- sumseries = sum . descend

{-
  sumseries 0
= sum (descend 0)
= sum []
= 0

  sumseries n
= sum (descend n)
= sum (n : descend (n - 1))
= n + sum (descend (n - 1))
= n + sumseries (n - 1)
-}

sumseries :: Nat -> Nat
sumseries 0 = 0
sumseries n = n + sumseries (n - 1)

-- (b)
-- repeatN (n, x) = map (const x) (descend n)

{-
  repeatN (0, x)
= map (const x) (descend 0)
= map (const x) []
= []

  repeatN (n, x)
= map (const x) (n : descend (n - 1))
= (const x) n : map (const x) (descend (n - 1))
= x : repeatN ((n - 1), x)
-}

repeatN :: (Nat, a) -> List a
repeatN (0, x) = []
repeatN (n, x) = x : repeatN ((n - 1), x)

-- (c)
-- rld = concat . map repeatN

{-
  rld []
= concat (map repeatN [])
= concat []
= []

  rld (x : xs)
= concat (map repeatN (x : xs))
= concat (repeatN x : map repeatN xs)
= repeatN x ++ concat (map repeatN xs)
= repeatN x ++ rld xs
-}

rld :: List (Nat, a) -> List a
rld [] = []
rld (x : xs) = repeatN x ++ rld xs
