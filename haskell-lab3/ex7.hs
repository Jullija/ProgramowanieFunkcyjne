onlyEven [] = []
onlyEven (x:xs)
    | x `mod` 2 == 0 = x:onlyEven xs
    | otherwise      = onlyEven xs



filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [ x | x <- xs, p x]

-- let onlyEven = filter' (\x -> x `mod` 2 == 0) [1, 2, 3, 4, 5]
-- let onlyOdd = filter' (\x -> x `mod` 2 /= 0) [1, 2, 3, 4, 5]
-- let onlyUpper = filter' (\x -> isUpper(x)) ["Ala MA KoTa"] --coś nie działa, sprawdzić


length2 xs = [x | x <- xs, x `mod` 2 == 0]