doubleElems[]    = []
doubleElems (x:xs) = 2 * x : doubleElems xs

sqrElems[]    = []
sqrElems (x:xs) = x * x : doubleElems xs

import Data.Char

map' :: (a->b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

--doubleElems = map' (*2) [1, 2, 3, 4]
--sqrElems = map' (^2) [1, 2, 3, 4]
--lowerCase = map' isLower ["Ala ma KoTa"]

