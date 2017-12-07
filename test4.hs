import Prelude ()
import MiniPrelude

reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]
-- quadratic time

{-
revcat xs ys = reverse' xs ++ ys

  revcat [] ys
= reverse' [] ++ ys
= [] ++ ys
= ys

  revcat (x : xs) ys
= reverse' (x : xs) ++ ys
= (reverse' xs ++ [x]) ++ ys
= reverse' xs ++ ([x] ++ ys)
= revcat xs (x : ys)
-}

revcat :: List a -> List a -> List a
revcat [] ys = ys
revcat (x : xs) ys = revcat xs (x : ys)

reverse'' xs = revcat xs []
-- linear time


square :: Int -> Int
square x = x * x

sumsq [] = 0
sumsq (x : xs) = square x + sumsq xs
-- linear space for the stack of return addresses

{-
ssp xs n = n + sumsq xs

  ssp [] n
= n + sumsq []
= n + 0
= n

  ssp (x : xs) n
= n + sumsq (x : xs)
= n + (square x + sumsq xs)
= sumsq xs + (n + square x)
= ssp xs (n + square x)
-}

ssp :: List Int -> Int -> Int
ssp [] n = n
ssp (x : xs) n = ssp xs (n + square x)

sumsq' xs = ssp xs 0
-- constant space


steep :: List Int -> Bool
steep [] = True
steep (x : xs) = steep xs && x > sum xs

{-
steepsum xs = (steep xs, sum xs)

  steepsum []
= (steep [], sum [])
= (True, 0)

  steepsum (x : xs)
= (steep (x : xs), sum (x : xs))
= (steep xs && x > sum xs, x + sum xs)
= (steep xs && x > sum xs, x + sum xs)
= ((fst . steepsum) xs && x > (snd . steepsum xs), x + (snd . steepsum xs))
-}

steepsum :: List Int -> (Bool, Int)
steepsum [] = (True, 0)
steepsum (x : xs) = (fst tmp && x > (snd tmp), x + (snd tmp))
  where tmp = steepsum xs

steep' = fst . steepsum
