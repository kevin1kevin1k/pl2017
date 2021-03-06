-- module Main where
import Prelude ()
import MiniPrelude
import M2
import Test.QuickCheck

{- Your code here -}
sol1 xs = [drop i xs ++ take i xs | i <- [(length xs),(length xs - 1)..1]]

{- Test your code using quickCheck -}

correct0 :: (List Int -> List (List Int)) -> List Int -> Bool
correct0 f xs = f xs == f0 xs

correct1 :: (Int -> List Int -> List Int) -> Int -> List Int -> Bool
correct1 f n xs = f n xs == f1 n xs
