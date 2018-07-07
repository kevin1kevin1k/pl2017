-- module Main where
import Prelude ()
import MiniPrelude
import MChallenge
import Test.QuickCheck

{- Your code here -}
-- positions :: Eq a => a -> List a -> List Int
-- positions x xs = map snd (filter ((x ==) . fst) (zip xs [0..]))
-- 
-- pos :: Eq a => a -> List a -> Int
-- pos x = head . positions x
-- 
-- sol1 xs ys =
--   drop (pos xs [(take len (drop i ys)) | i <- [0..(length ys - len)]]) ys
--   where len = length xs


sol1 xs ys = answer
  where len = length xs
        indices = [0..(length ys - len)]
        sublists = [(take len (drop i ys)) | i <- indices]
        positions x xs = map snd (filter ((x ==) . fst) (zip xs [0..]))
        pos x = head . positions x
        first_pos = (pos xs sublists)
        answer = drop first_pos ys

-- *Main> sol1 [1, 2] [0, 4, 3, 1, 5, 1, 2, 7, 2, 1, 2]
-- [1,2,7,2,1,2]

{- Test your code using quickCheck -}

correct0 :: (List Int -> List Int -> List Int) -> List Int -> List Int -> Bool
correct0 f xs ys = f xs ys == find xs ys
