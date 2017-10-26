-- single-line comment

{-
multi-line
comments
haha
-}

smaller :: Int -> Int -> Int
smaller x y =
  if x < y then x
           else y

square :: Int -> Int
square x = x * x

three :: Int -> Int
three x = 3

infinity :: Int
infinity = infinity + 1

myeven :: Int -> Bool
myeven x = mod x 2 == 0

circle_area :: Double -> Double
circle_area r = let my_pi = 22 / 7
                in my_pi * r * r
                
smaller_guarded :: Int -> Int -> Int
smaller_guarded x y | x < y = x
                    | otherwise = y

payment_let :: Int -> Int
payment_let weeks =
  let days_per_week = 5
      hours_per_day = 8
      pay_per_hour = 130
  in weeks * days_per_week * hours_per_day * pay_per_hour
        
payment_where :: Int -> Int
payment_where weeks =
  weeks * days_per_week * hours_per_day * pay_per_hour
  where days_per_week = 5
        hours_per_day = 8
        pay_per_hour = 130

payment_where2 :: Int -> Int
payment_where2 weeks | weeks > 19 = round (fromIntegral pay * 1.06)
                     | otherwise = pay
  where pay = payment_where weeks

nested :: Int
nested = let x = 3
         in (let x = 5
             in x + x) + x

recursive :: Int
recursive = let x = 3
            in let x = x + 1
               in x

st3 = smaller 3

poly a b c x = a * x^2 + b * x + c

poly1 = poly 1 2 1

poly2 a b c = poly a b c 2

quad x = square (square x)

twice1 f x = f (f x)

quad1 = twice1 square

twice2 f = f.f

quad2 = twice2 square

increment = (1 +)

reciprocal = (1.0 /)

halve = (/ 2.0)

forktimes f g x = f x * g x
f132 = forktimes (+1) (+2)

lift2 h f g x = h (f x) (g x)
f132_2 = lift2 (*) (+1) (+2) 

-- Proof of (f.g).h == f.(g.h)
-- (f.g).h x == f.g (h x) == f (g (h x))
-- f.(g.h) x == f (g.h x) == f (g (h x))

