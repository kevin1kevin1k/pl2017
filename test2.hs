isDigit :: Char -> Bool
isDigit c = '0' <= c && c <= '9'

-- head xs ++ tail

double = (*2)

doubleAll = map double
doubleAll2 = map (\x -> 2 * x)

quadAll = doubleAll . doubleAll
quadAll2 = map (double . double)

-- for any functions f, g,
-- map (f . g) == map f . map g

swap = \(x, y) -> (y, x)
swap2 = \p -> (snd p, fst p)

squaresUpTo n = takeWhile (<=n) (map (^2) [1..])

positions ch str = map snd (filter ((ch ==) . fst) (zip str [0..]))
positions2 ch str = [i | (c, i) <- zip str [0..], c == ch]

pos ch = head . positions ch




