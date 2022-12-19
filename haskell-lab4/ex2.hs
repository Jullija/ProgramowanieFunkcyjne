-- product type example (one constructor)
data CartInt2DVec = MkCartInt2DVec Int Int -- konwencja: prefix 'Mk' dla konstruktora

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y



data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y



data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

-- xCoord'' :: Cart2DVec'' a -> a
-- xCoord'' (MkCart2DVec'' {x = xVal, y = _}) = xVal

-- yCoord'' :: Cart2DVec'' a -> a
-- yCoord'' (MkCart2DVec'' {y = yVal, x = _}) = yVal -- uwaga na kolejność x,y


data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x


data ThreeColors = Blue |
                   White |
                   Pink

type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue  = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Pink   = "Irene Jacob"




--zad1
--konstrukcja typu    konstrukcja danych
data Cart3DVec' a = Cart3DVec' {x1::a, y1::a, z1::a}
xCoord3D' :: Cart3DVec' a -> a;
xCoord3D' (Cart3DVec' {x1 = xVal, y1 = _, z1 = _}) = xVal;
yCoord3D' :: Cart3DVec' a -> a;
yCoord3D' (Cart3DVec' {x1 = _, y1 = yVal, z1 = _}) = yVal;
zCoord3D' :: Cart3DVec' a -> a;
zCoord3D' (Cart3DVec' {x1 = _, y1 = _, z1 = zVal}) = zVal;

data Cart3DVec''  = Cart3DVec'' Int Int Int 
xCoord3D'' :: Cart3DVec''-> Int
xCoord3D'' (Cart3DVec'' x2 _ _) = x2
yCoord3D'' :: Cart3DVec''-> Int
yCoord3D'' (Cart3DVec'' _ y2 _) = y2
zCoord3D'' :: Cart3DVec''-> Int
zCoord3D'' (Cart3DVec'' _ _ z2) = z2



--zad6

data Shape = Circle Float |
             Rectangle Float Float


area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle x y ) = x * y


--zad8

data TrafficLights = Red |
                     Orange |
                     Green
                    

actionFor :: TrafficLights -> String
actionFor Red = "Stoj"
actionFor Orange = "Zaraz bedzie czerwone"
actionFor Green = "Jedz"