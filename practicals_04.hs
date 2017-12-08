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


-- (2)

-- pos x = length . takeWhile (x /=)

{-
  pos x []
= length (takeWhile (x /=) [])
= length []
= 0

  pos x (x : ys)
= length (takeWhile (x /=) (x : ys))
= length (takeWhile (x /=) (x : ys))
= length []
= 0

  pos x (y : ys)
= length (takeWhile (x /=) (y : ys))
= length (y : takeWhile (x /=) ys)
= 1 + pos x ys
-}

pos :: Eq a => a -> List a -> Int
pos x [] = 0
pos x (y : ys) =
  if x == y then 0
            else 1 + pos x ys


-- (3)

-- (a)

second :: (b -> c) -> (a, b) -> (a, c)
second f (x, y) = (x, f y)

{-
Proof of
zip xs (map f ys) == map (second f) (zip xs ys)

  map (second f) (zip [] [])
= map (second f) []
= []
= zip [] []
= zip [] (map f [])

  map (second f) (zip (x : xs) (x : ys))
= map (second f) ((x, y) : zip xs ys)
= (second f) (x, y) : map (second f) (zip xs ys)
= (x, f y) : zip xs (map f ys)
= (x, f y) : zip xs (map f ys)
= zip (x : xs) (f y : map f ys)
= zip (x : xs) (map f (y : ys))
-}

-- (b)

delete :: List a -> List (List a)
delete [] = []
delete (x : xs) = xs : map (x :) (delete xs)

-- select xs = zip xs (delete xs)

{-
  select []
= zip [] (delete [])
= zip [] []
= []

  select (x : xs)
= zip (x : xs) (delete (x : xs))
= zip (x : xs) (xs : map (x :) (delete xs))
= (x, xs) : zip xs (map (x :) (delete xs))
= (x, xs) : map (second (x :)) (zip xs (delete xs))
= (x, xs) : map (second (x :)) (select xs)
-}

select :: List a -> List (a, List a)
select [] = []
select (x : xs) = (x, xs) : map (second (x :)) (select xs)

-- (c)

delete' xs = map (del xs) [0..(length xs - 1)]
  where del xs i = take i xs ++ drop (1 + i) xs

-- [0..n] == 0 : map (1 +) [0..(n - 1)]

-- map f . map g == map (f . g)
-- Proved in (4) below

{-
Lemma
del (x : xs) . (1 +) == (x :) . del xs

  del (x : xs) ((1 +) 0)
= del (x : xs) 1
= take 1 (x : xs) ++ drop (1 + 1) (x : xs)
= [x] ++ drop 1 xs
= (x :) (drop 1 xs)
= (x :) ([] ++ drop 1 xs)
= (x :) (take 0 xs ++ drop 1 xs)
= (x :) (del xs 0)

  del (x : xs) ((1 +) i)
= take (i + 1) (x : xs) ++ drop (1 + i + 1) (x : xs)
= (x : take i xs) ++ drop (i + 1) xs
= (x : take i xs) ++ drop (i + 1) xs
= x : (take i xs ++ drop (i + 1) xs)
= x : del xs i
= (x :) (del xs i)
-}

{-
  delete' []
= map (del []) [0..(length [] - 1)]
= map (del []) [0..(-1)]
= map (del []) []
= []

delete' (x : xs)
= map (del (x : xs)) [0..(length (x : xs) - 1)]
= map (del (x : xs)) [0..(length xs)]
= map (del (x : xs)) (0 : map (1 +) [0..(length xs - 1)])
= del (x : xs) 0 : map (del (x : xs)) (map (1 +) [0..(length xs - 1)])
= (take 0 (x : xs) ++ drop (1 + 0) (x : xs)) : map (del (x : xs) . (1 +)) [0..(length xs - 1)]
= ([] ++ xs) : map (del (x : xs) . (1 +)) [0..(length xs - 1)]
= xs : map (del (x : xs) . (1 +)) [0..(length xs - 1)]
= xs : map ((x :) . del xs) [0..(length xs - 1)]
= xs : map (x :) (map (del xs) [0..(length xs - 1)])
= xs : map (x :) (delete' xs)
-}


-- (4)

{-
Proof of
map f . map g == map (f . g)

  (map f . map g) []
= map f (map g [])
= map f []
= []
= map (f . g) []

  (map f . map g) (x : xs)
= map f (map g (x : xs))
= map f (g x : map g xs)
= (f . g) x : map f (map g xs)
= (f . g) x : map (f . g) xs
= map (f . g) (x : xs)
-}


-- (5)

filter' :: (a -> Bool) -> List a -> List a
filter' p [] = []
filter' p (x : xs) = if p x then x : filter' p xs
                           else filter' p xs

-- f (if q then e1 else e2) == if q then f e1 else f e2

{-
Proof of
filter' p . map f = map f . filter' (p . f)

  filter' p (map f [])
= filter' p []
= []

  filter' p (map f (x : xs))
= filter' p (f x : map f xs)
= if p (f x) then f x : filter' p (map f xs) else filter' p (map f xs)
= if p (f x) then f x : map f (filter' (p . f) xs) else map f (filter' (p . f) xs)
= if p (f x) then map f (x : filter' (p . f) xs) else map f (filter' (p . f) xs)
= map f (if p (f x) then x : filter' (p . f) xs else filter' (p . f) xs)
= map f (if (p . f) x then x : filter' (p . f) xs else filter' (p . f) xs)
= map f (filter' (p . f) (x : xs))
-}


-- (11)

fact :: Nat -> Nat
fact 0 = 1
fact n = n * fact (n - 1)
-- linear space

{-
factit n m = m * fact n

  factit 0 m
= m * fact 0
= m * 1
= m

  factit n m
= m * fact n
= m * (n * fact (n - 1))
= (m * n) * fact (n - 1)
= factit (n - 1) (m * n)
= factit (n - 1) (m * n)
-}

factit :: Nat -> Nat -> Nat
factit 0 m = m
factit n m = factit (n - 1) (m * n)

fact' n = factit n 1
-- constant space


-- (12)

fib :: Nat -> Nat
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
-- exponential time

-- fastfib n x y = x * fib n + y * fib (n + 1)

{-
  fastfib 0 x y
= x * fib 0 + y * fib 1
= x * 0 + y * 1
= y

  fastfib n x y
= x * fib n + y * fib (n + 1)
= x * fib n + y * (fib n + fib (n - 1))
= y * fib (n - 1) + (x + y) * fib n
= fastfib (n - 1) y (x + y)
-}

fastfib :: Nat -> Nat -> Nat -> Nat
fastfib 0 x y = y
fastfib n x y = fastfib (n - 1) y (x + y)

fib' n = fastfib n 1 0
-- linear time
