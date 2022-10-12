not' :: Bool -> Bool
not' b = case b of 
        True -> False
        False -> True

absInt :: Int -> Int
absInt n = 
    case (n >= 0) of
        True -> n 
        _ -> -n



isItTheAnswer :: String -> Bool
isItTheAnswer x = case x of
                "Love" -> True
                _ -> False


or' :: (Bool, Bool) -> Bool
or' x = case x of
        (False, False) -> False
        _ -> True


or'2 :: (Bool, Bool) -> Bool
or'2 (x, y) = case (x, y) of
              (False, False) -> False
              (_, _) -> True        


and' :: (Bool, Bool) -> Bool
and' x = case x of
        (True, True) -> True
        _ -> False

nand' :: (Bool, Bool) -> Bool
nand' x = case x of
          (True, True) -> False
          _ -> True

xor' :: (Bool, Bool) -> Bool
xor' x = case x of
         (True, True) -> False
         (False, False) -> False
         _ -> True
