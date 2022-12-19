import Data.Char (toUpper)
isPalindrome :: [Char] -> Bool
isPalindrome s  | s == reverse s = True
                | otherwise = False


--NIE DZIAŁA :(

-- getElemAtIdx tab idx = if tab length == 0 --pusta tablica
--                         then 0
--                         else if tab length == 1 || idx == 0 --jednoelementowa lub index 1
--                             then head tab
--                             else if idx == tab length --koniec tablicy
--                                 then tail tab
--                                 else let ans = drop idx tab;
--                                           ans2 = reverse ans;
--                                           ans3 = drop idx ans2
--                                     in ans3


-- getElemAtIdx2 tab idx | tab == [] = 0 --pusta tablica
--                       | length tab == 1 = head tab--jednoelementowa
--                       | idx == 1 = head tab--index to początek
--                       | idx == length tab = tail tab --index to koniec
--                       | otherwise = {let ans = drop idx tab; ans2 = reverse ans; ans3 = drop idx ans2} in ans3



capitalize :: [Char] -> [Char]
capitalize [] = []
capitalize w | (fromEnum (head w) :: Int) >= 65 && (fromEnum (head w) :: Int) <= 90 = w
             | otherwise = (toEnum ((fromEnum(head w) :: Int) - 32) :: Char) : tail w
-- capitalize w  = toUpper(head w) : tail w
