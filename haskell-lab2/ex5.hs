--Napisać wyrażenie obliczające, ile jest w przedziale [1,100] trójek liczb całkowitych reprezentujących długości boków trójkąta prostokątnego
--length [(i, j, k) | i <- [1..100], j <- [i..100], k <- [i..100], i^2 + j^2 == c^2]

isPrime :: Integral t => t -> Bool
isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == []
--niepoprawna, bo dla 0 i 1 zwraca liczbę pierwszą
--isPrime k = if k > 1 then null [ x | x <- [2..isqrt k], k `mod` x == 0] else False


--Napisać wyrażenie obliczające, ile jest w przedziale [1,10000] liczb pierwszych
--length [n | n <- [2..1000], length [y | y <- [2..n-1], n `mod` y == 0] == 0


primes :: [Int]
primes = eratoSieve [2..]
 where
   eratoSieve :: [Int] -> [Int]
   eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]

