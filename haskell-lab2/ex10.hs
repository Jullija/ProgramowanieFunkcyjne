fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False



firstDivSecond :: Integral a => [a] -> Bool
firstDivSecond (x : y : _) | y `mod` x == 0 = True
firstDivSecond _                            = False



firstDivThird :: Integral a => [a] -> Bool
firstDivThird (x : y : z : _) | z `mod` x == 0 = True
firstDivThird _                                = False