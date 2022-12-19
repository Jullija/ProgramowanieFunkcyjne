qSort :: Ord a => [a] -> [a]
qSort []     = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
 where
   leftPart  xs = [ y | y <- xs, y <= x ]
   rightPart xs = [ y | y <- xs, y > x  ]



qSort2 :: Ord a => [a] -> [a]
qSort2 []     = []
qSort2 (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
 where
   leftPart  xs = filter ( <= x) (xs)
   rightPart xs = filter ( > x) (xs)




--Merge Sort
mSort'merge :: (Ord a) => [a] -> [a] -> [a]
mSort'merge [] xs = xs
mSort'merge xs [] = xs
mSort'merge (x:xs) (y:ys)
    | (x < y) = x : mSort'merge xs (y:ys)
    | otherwise = y:mSort'merge (x:xs) ys


mSort'splitinhalf :: [a] -> ([a], [a])
mSort'splitinhalf xs = (take n xs, drop n xs)
    where n = (length xs) `div` 2 


mSort :: (Ord a) => [a] -> [a]
mSort [] = []
mSort [x] = [x]
mSort xs = mSort'merge (mSort left) (mSort right)
 where (left, right) = mSort'splitinhalf xs





--Insertion Sort
insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) |x < y = x : y : ys
                |otherwise = y : (insert x ys)


iSort :: (Ord a) => [a] -> [a]
iSort [] = []
iSort (x:xs) = insert x (iSort xs)



