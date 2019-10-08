module ChapterEleven where

data Price = Price Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)
-- data Vehicle = Car Manufacturer Price | Plane Airline deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir (Size 5000)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars [Car _ _] = [True]
areCars _ = [False]

getManu :: Vehicle -> Manufacturer
getManu (Car man _) = man

data Size = Size Integer deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)


-- logic goats

-- {-# LANGUAGE FlexibleInstances  #-}

-- class TooMany a where
--     tooMany :: a -> Bool

-- instance TooMany (Int, String) where
--     tooMany (n, _) = n > 42

-- instance TooMany (Int, Int) where
--     tooMany (a, b) = (a + b) > 42



-- binary tree exercises
data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
    Node (Node Leaf 3 Leaf)
        1
        (Node Leaf 4 Leaf)

mapExpected =
    Node (Node Leaf 4 Leaf)
        2
        (Node Leaf 5 Leaf)

mapOkay =
    if mapTree (+1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = x ++ y ++ z
        where
            x = [a]
            y = preorder left
            z = preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = x ++ y ++ z
        where
            x = preorder left
            y = [a]
            z = preorder right
    
postorder :: BinaryTree a -> [a]
postorder Leaf  = []
postorder (Node left a right) = x ++ y ++ z
        where
            x = postorder left
            y = postorder right
            z = [a]
    
testTree :: BinaryTree Integer
testTree =
    Node (Node Leaf 1 Leaf)
        2
        (Node Leaf 3 Leaf)
    
testPreorder :: IO ()
testPreorder =
    if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."
    
testInorder :: IO ()
testInorder =
    if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."
    
testPostorder :: IO ()
testPostorder =
    if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "postorder failed check"

main :: IO ()
main = do
    testPreorder
    testInorder
    testPostorder