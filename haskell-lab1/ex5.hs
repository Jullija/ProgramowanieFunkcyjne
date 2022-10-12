
sgn :: Int -> Int
sgn n = if n < 0
        then -1
        else if n == 0
            then 0
            else 1



absInt :: Int -> Int
absInt n = if n < 0
            then (-n)
            else n


min2Int :: (Int, Int) -> Int
min2Int (x, y) = if x < y
                then x
                else y



min3Int :: (Int, Int, Int) -> Int
min3Int (x, y, z) = if x <= y && x <= z
                    then x
                    else if y <= x && y <= z
                        then y
                        else z



min3Int2 :: (Int, Int, Int) -> Int
min3Int2 (x, y, z) = min2Int(x, min2Int(y, z))



toUpper :: Char -> Char
toUpper x = if (fromEnum x :: Int) >= 65 && (fromEnum x :: Int) <= 90
            then x
            else toEnum ((fromEnum x :: Int) - 32) :: Char


toLower :: Char -> Char
toLower x = if (fromEnum x :: Int) >= 97 && (fromEnum x :: Int) <= 122
            then x
            else toEnum ((fromEnum x :: Int) + 32) :: Char


isDigit :: Char -> Bool
isDigit x = if (fromEnum x :: Int) >= 48 && (fromEnum x :: Int) <= 57
            then True
            else False


charToNum :: Char -> Int
charToNum x = if isDigit(x) == False
              then fromEnum x :: Int
              else if x == '1'
                then 1
                else if x == '2'
                    then 2
                    else if x == '3'
                        then 3
                        else if x == '4'
                            then 4
                            else if x == '5'
                                then 5
                                else if x == '6'
                                    then 6
                                    else if x == '7'
                                        then 7
                                        else if x == '8'
                                            then 8
                                            else if x =='9'
                                                then 9
                                                else 0


romanDigit :: Char -> String
romanDigit x = if x == '1'
                then "I"
                else if x == '2'
                    then "II"
                    else if x == '3'
                        then "III"
                        else if x == '4'
                            then "IV"
                            else if x == '5'
                                then "V"
                                else if x == '6'
                                    then "VI"
                                    else if x == '7'
                                        then "VII"
                                        else if x == '8'
                                            then "VIII"
                                            else "IX"