absInt :: Int -> Int
absInt n | n >= 0 = n
         | n < 0 = -n



sgn :: Int -> Int
sgn n | n < 0 = -1
      | n == 0 = 0
      | n > 0 = 1 --otherwise = 1


min3Int :: (Int, Int, Int) -> Int
min3Int (x, y, z) | x <= y && x <= z = x
                  | y <= x && y <= z = y
                  | otherwise = z



toUpper :: Char -> Char
toUpper x | (fromEnum x :: Int) >= 65 && (fromEnum x :: Int) <= 90 = x
          | otherwise = toEnum ((fromEnum x :: Int) - 32) :: Char



toLower :: Char -> Char
toLower x | (fromEnum x :: Int) >= 97 && (fromEnum x :: Int) <= 122 = x
          | otherwise = toEnum ((fromEnum x :: Int) + 32) :: Char



isDigit :: Char -> Bool
isDigit x | (fromEnum x :: Int) >= 48 && (fromEnum x :: Int) <= 57 = True
          | otherwise = False



charToNum :: Char -> Int
charToNum x | isDigit(x) == False = fromEnum x :: Int
            | x == '1' = 1
            | x == '2' = 2
            | x == '3' = 3
            | x == '4' = 4
            | x == '5' = 5
            | x == '6' = 6
            | x == '7' = 7
            | x == '8' = 8
            | x == '9' = 9
            | otherwise = 0


romanDigit :: Char -> String
romanDigit x | x == '1' = "I"
             | x == '2' = "II"
             | x == '3' = "III"
             | x == '4' = "IV"
             | x == '5' = "V"
             | x == '6' = "VI"
             | x == '7' = "VII"
             | x == '8' = "VIII"
             | otherwise = "IX"




