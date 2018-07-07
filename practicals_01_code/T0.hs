-- module Main where
import Prelude()
import MiniPrelude
import M0
import Test.QuickCheck

{- Your code here -}
sol1 xs = (drop 1 . take ((length xs) * 2 - 1)) (concat [[i, i] | i <- xs])

sol2 xs
  | null xs = []
  | otherwise = tail . init $ (concat [[i, i] | i <- xs])

{- Test your code using quickCheck -}

correct0 :: (List Int -> List Int) -> List Int -> Bool
correct0 f xs = f xs == f0 xs

correct1 :: ((Int, Int) -> List Int) -> (Int, Int) -> Bool
correct1 f (x,y) = f (x,y) == f1 (x,y)

correct2 :: (List Int -> List (Int, Int)) -> List Int -> Bool
correct2 f xs = f xs == f2 xs
