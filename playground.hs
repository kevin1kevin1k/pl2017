compose f n
  | n == 0 = id
  | otherwise = (compose f (n-1)) . f

power2All n = map (compose (*2) n)

isPrime n
  | n <= 1 = False
  | otherwise = all (\i -> mod n i /= 0) [2 .. n-1]
  -- | otherwise = all (\i -> mod n i /= 0) [2 .. round (sqrt n)]

primesUpTo n = filter isPrime [1..n]
