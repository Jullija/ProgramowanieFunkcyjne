isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc xs = and $ zipWith (<) xs (tail xs)


everySecond :: [t] -> [t] --tu się zastanów Julia
everySecond xs = 