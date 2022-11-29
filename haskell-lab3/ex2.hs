sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x +  sum' xs

sumSqr' [] = 0
sumSqr' (x:xs) = x * x + sumSqr' xs

sumWith :: Num a => (a->a) -> [a] -> a
sumWith f [] = 0
sumWith f (x:xs) = f x + sumWith f xs

sum2 = sumWith (\x -> x)
sumSqr = sumWith (\x -> x*x)
sumCube = sumWith (\x -> x*x*x)
sumAbs = sumWith (\x -> abs(x))

--Sigma i=1 n = 15 i^5
sigma = sumWith (\x -> x*x*x*x*x ) [1..15] --jeszcze do poprawy

listLength = sumWith (\x -> 1)


prod' :: Num a => [a] -> a
prod' [] = 1
prod' (x:xs) = x * prod' xs

prodWith :: Num a => (a->a) -> [a] -> a
prodWith f [] = 1
prodWith f (x:xs) = f x * prodWith f xs

prod2 = prodWith (\x -> x)
prodSqr = prodWith (\x -> x*x)
prodCube = prodWith (\x -> x*x*x)
prodAbs = prodWith (\x -> abs(x))