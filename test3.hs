import Prelude ()
import MiniPrelude
-- import Data.List

sum' :: List Int -> Int
sum' []       = 0
sum' (x : xs) = x + sum' xs

map' :: (a -> b) -> List a -> List b
map' f []       = []
map' f (x : xs) = f x : map' f xs

(+++) :: List a -> List a -> List a
[]       +++ ys = ys
(x : xs) +++ ys = x : (xs +++ ys)

length' :: List a -> Int
length' []       = 0
length' (x : xs) = 1 + length' xs

concat' :: List (List a) -> List a
concat' []         = []
concat' (xs : xss) = xs +++ (concat' xss)

foldr' :: (a -> b -> b) -> b -> List a -> b
foldr' f e []       = e
foldr' f e (x : xs) = f x (foldr' f e xs)
-- f e [1..3] = f 1 (f 2 (f 3 e))

foldl' :: (b -> a -> b) -> b -> List a -> b
foldl' f e []       = e
foldl' f e (x : xs) = foldl' f (f e x) xs
-- f e [1..3] = f (f (f e 1) 2) 3


sum'' = foldr' (+) 0
length'' = foldr' (\x n -> 1 + n) 0
concat'' = foldr' (+++) []


filter' :: (a -> Bool) -> List a -> List a
filter' p []       = []
filter' p (x : xs) = (if p x then [x] else []) +++ (filter' p xs)

take' :: Int -> List a -> List a
take' 0 xs       = []
take' n []       = []
take' n (x : xs) = x : take' (n - 1) xs

drop' :: Int -> List a -> List a
drop' 0 xs       = xs
drop' n []       = []
drop' n (x : xs) = drop' (n - 1) xs

takeWhile' :: (a -> Bool) -> List a -> List a
takeWhile' p []       = []
takeWhile' p (x : xs) = if p x then x : takeWhile' p xs else []

dropWhile' :: (a -> Bool) -> List a -> List a
dropWhile' p []       = []
dropWhile' p (x : xs) = if p x then dropWhile' p xs else x : xs

reverse' :: List a -> List a
reverse' []       = []
reverse' (x : xs) = reverse' xs +++ [x]

inits' :: List a -> List (List a)
inits' []       = [[]]
inits' (x : xs) = [] : map' (x :) (inits' xs)

tails' :: List a -> List (List a)
tails' []       = [[]]
tails' (x : xs) = (x : xs) : tails' xs

fib' :: Int -> Int
fib' 0 = 0
fib' 1 = 1
fib' n = (fib' (n - 1)) + (fib' (n - 2))

merge' :: List Int -> List Int -> List Int
merge' [] ys             = ys 
merge' xs []             = xs
merge' (x : xs) (y : ys) = if x < y then x : (merge'      xs (y : ys))
                                    else y : (merge' (x : xs)     ys)

zip' :: List a -> List b -> List (a, b)
zip' xs []             = []
zip' [] ys             = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys
