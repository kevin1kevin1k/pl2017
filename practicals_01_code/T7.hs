-- module Main where
import Prelude ()
import MiniPrelude
import M7
import Test.QuickCheck

{- Your code here -}
sol1 n xs = [(xs !! i, xs !! (i+1)) | i <- [0..(length xs - 2)], xs !! i == n]

{- Test your code using quickCheck -}

correct0 :: (Int -> List Int -> List (Int,Int)) -> Int -> List Int -> Bool
correct0 f x xs = f x xs == f0 x xs

correct1 :: (List Int -> List (Int,Int)) -> List Int -> Bool
correct1 f xs = f xs == f1 xs
