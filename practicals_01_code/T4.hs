-- module Main where
import Prelude ()
import MiniPrelude
import M4
import Test.QuickCheck

{- Your code here -}
sol1 xs = [take i xs | i <- [0..length xs]]

{- Test your code using quickCheck -}

correct0 :: ([Int] -> [[Int]]) -> [Int] -> Bool
correct0 f xs = f xs == f0 xs

correct1 :: ([Int] -> [Int]) -> [Int] -> Bool
correct1 f xs = f xs == f1 xs
