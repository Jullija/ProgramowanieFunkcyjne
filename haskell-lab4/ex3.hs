data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt



data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt




data Expr a = Lit a | -- literal/value a, e.g. Lit 2 = 2
              Add (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"




--zad1
depthOfBT :: BinTree a -> Int -- głębokość drzewa binarnego
depthOfBT EmptyBT = 0
depthOfBT (NodeBT n lt rt) = max(1 + depthOfBT lt) (1 + depthOfBT rt)

flattenBTPreorder :: BinTree a -> [a]  -- napisać trzy wersje: preorder, inorder, postorder
flattenBTPreorder EmptyBT = []
flattenBTPreorder (NodeBT n lt rt) = [n] ++ flattenBTPreorder lt ++ flattenBTPreorder rt




quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort (leftPart xs) ++ [x] ++ quicksort (rightPart xs)
 where
   leftPart  xs = filter ( <= x) (xs)
   rightPart xs = filter ( > x) (xs)


flattenBTInorder :: Ord a => BinTree a -> [a]  -- napisać trzy wersje: preorder, inorder, postorder
flattenBTInorder EmptyBT = []
flattenBTInorder (NodeBT n lt rt) = quicksort $ ([n] ++ flattenBTPreorder lt ++ flattenBTPreorder rt)



flattenBTPostorder ::  BinTree a -> [a]  -- napisać trzy wersje: preorder, inorder, postorder
flattenBTPostorder EmptyBT = []
flattenBTPostorder (NodeBT n lt rt) = flattenBTPostorder lt ++ [n] ++ flattenBTPostorder rt



--mapBT :: (a -> b) -> BinTree a -> BinTree b -- funkcja map dla drzewa binarnego
-- mapBT :: (a->b) -> BinTree a -> BinTree b
-- mapBT f EmptyBT = EmptyBT
-- mapBT f (NodeBT n lt rt) = NodeBT (f n) (mapBT lt) (mapBT rt)



--insert :: Ord a => a -> BinTree a -> BinTree a -- insert element into BinTree

insert :: Ord a => a -> BinTree a -> BinTree a
insert x EmptyBT = NodeBT x EmptyBT EmptyBT
insert x (NodeBT e lt rt) 
  | x == e = NodeBT e lt rt
  | x < e = insert x lt
  | otherwise = insert x rt



-- list2BST :: Ord a => [a] -> BinTree a -- list to Binary Search Tree (BST)