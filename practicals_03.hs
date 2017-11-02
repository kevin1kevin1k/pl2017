import Prelude ()
import MiniPrelude


-- 1
{-
Proof of
length (xs ++ ys) == length xs + length ys

  length ([] ++ ys)
= length ys -- def. of ++
= 0 + length ys -- additive identity
= length [] + length -- def. of length

  length ((x : xs) ++ ys)
= length (x : (xs ++ ys)) -- def. of ++
= 1 + length (xs ++ ys) -- def. of length
= 1 + (length xs + length ys) -- induction hypothesis
= (1 + length xs) + length ys -- additive associativity
= (length (x : xs)) + length ys -- def. of length
-}


-- 2
{-
Proof of
sum . concat == sum . map sum

Lemma
sum (xs ++ ys) == sum xs + sum ys

  (sum . concat) []
= sum (concat []) -- def. of .
= sum [] -- def. of concat
= sum (map sum []) -- def. of map
= (sum . map sum) [] -- def. of .

  (sum . concat) (xs : xss)
= sum (concat (xs : xss)) -- def. of .
= sum (xs ++ concat xss) -- def. of concat
= sum xs + sum (concat xss) -- lemma
= sum xs + (sum . concat) xss -- def. of .
= sum xs + (sum . map sum) xss -- induction hypothesis
= sum xs + sum (map sum xss) -- def. of .
= sum (sum xs : map sum xss) -- def. of sum
= sum (map sum (xs : xss)) -- def. of map
= (sum . map sum) (xs : xss) -- def. of .
-}

-- 3
{-
Proof of
take n xs ++ drop n xs == xs

  take n [] ++ drop n []
= [] ++ [] -- def. of take and drop
= [] -- def. of ++

  take 0 (x : xs) ++ drop 0 (x : xs)
= [] ++ (x : xs) -- def. of take and drop
= x : xs -- def. of ++

  take n (x : xs) ++ drop n (x : xs) -- n > 0
= (x : take (n - 1) xs) ++ (drop (n - 1) xs) -- def. of take and drop
= x : ((take (n - 1) xs) ++ (drop (n - 1) xs)) -- def. of ++
= x : xs -- induction hypothesis
-}

-- 4
fan :: a -> List a -> List (List a)
fan x [] = [[x]]
fan x (y : ys) = (x : y : ys) : map (y :) (fan x ys)

-- 5
perms :: List a -> List (List a)
perms [] = [[]]
perms (x : xs) = concat (map (fan x) (perms xs))

-- 6
splits :: List a -> List (List a, List a)
splits [] = [([], [])]
splits (x : xs) = ([], x : xs) : map cons_fst (splits xs)
  where cons_fst = \(a, b) -> ((x : a), b)

-- 7 and 8 are in test3.hs

-- 9
interleave :: List a -> List a -> List (List a)
interleave [] [] = []
interleave xs [] = [xs]
interleave [] ys = [ys]
interleave (x : xs) (y : ys) = xx ++ yy
  where xx = map (x :) (interleave xs (y : ys))
        yy = map (y :) (interleave (x : xs) ys)

-- 10
sublist :: List a -> List (List a)
sublist [] = [[]]
sublist (x : xs) = sub ++ map (x :) sub
  where sub = sublist xs
