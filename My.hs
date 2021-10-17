--
-- EPITECH PROJECT, 2021
-- B-FUN-300-STG-3-1-funPoolDay1-clement.muth
-- File description:
-- My
--

mySucc :: Int -> Int
mySucc x = x + 1

myIsNeg :: Int -> Bool
myIsNeg x
    | x < 0 = True
    | otherwise = False

myAbs :: Int -> Int
myAbs n | n >= 0 = n
    | otherwise = -n

myMin :: Int -> Int -> Int
myMin a b | a <= b = a
    | otherwise = b

myMax :: Int -> Int -> Int
myMax a b | a >= b = a
    | otherwise = b

myTuple :: a -> b -> (a, b)
myTuple a b = (a, b)

myTruple :: a -> b -> c -> (a, b, c)
myTruple a b c = (a, b, c)

myFst :: (a, b) -> a
myFst (a, b) = a

mySnd :: (a, b) -> b
mySnd (a, b) = b

mySwap :: (a , b) -> (b, a)
mySwap (a, b) = (b, a)

myHead :: [a] -> a
myHead (x:_) = x
myHead [] = error "no tete"

myTail :: [a] -> [a]
myTail (_:x) = x
myTail [] = error "no queue"

myLength :: [a] -> Int
myLength [] = 0
myLength (_:x) = 1 + myLength x

myIsEqZero :: Int -> Bool
myIsEqZero n
    | n <= 0 = True
    | otherwise = False

myNth :: [a] -> Int -> a
myNth [] x = error "Empty list"
myNth (y:ys) x
    | myLength(ys) < x = error "Too large number"
    | myIsNeg(x) == True = error "Negative index"
    | myIsEqZero(x) = y
    | otherwise = myNth ys (x - 1)

myTake :: Int -> [a] -> [a]
myTake 0 _ = []
myTake x (ys)
    | myIsNeg(x) == True = error "Negative index"
    | myLength(ys) < x = ys
myTake x (y:ys) = y : myTake (x - 1) ys

myDrop :: Int -> [a] -> [a]
myDrop x xs
    | myIsNeg(x) == True = error "Negative index"
    | myIsEqZero(x) || myIsEqZero(myLength(xs)) = xs
    | otherwise = myDrop (x - 1) (myTail xs)

myAppend :: [a] -> [a] -> [a]
myAppend [] ys = ys
myAppend (x:xs) ys = x : myAppend xs ys

myInit :: [a] -> [a]
myInit [] = error "Empty list"
myInit xs = myTake (myLength xs - 1) xs


myLast :: [a] -> a
myLast [] = error "Empty list"
myLast (x:xs)
    | myLength(xs) >= 1 = myLast xs
    | otherwise = x

myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = myLast xs : myReverse (myInit xs)

myZip :: [a] -> [b] ->[(a, b)]
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys
myZip _ _ = []

myMap :: (a -> b) -> [a] -> [b]
myMap a [] = []
myMap a (x:xs) = a x : myMap a xs

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip xs = (myMap myFst xs, myMap mySnd xs)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter a (x:xs)
    | a x = x : myFilter a xs
    | otherwise = myFilter a xs
myFilter _ [] = []

myFilterNeg :: (a -> Bool) -> [a] -> [a]
myFilterNeg a (x:xs)
    | a x = myFilterNeg a xs
    | otherwise = x : (myFilterNeg a xs)
myFilterNeg _ [] = []

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl a b [] = b
myFoldl a b (x:xs) = myFoldl a (a b x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr a b [] = b
myFoldr a b (x:xs) = a x (myFoldr a b xs)

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition a [] = ([], [])
myPartition a b = (myFilter a b, myFilterNeg a b)

myQuickSort :: (a -> a -> Bool) -> [a] -> [a]
myQuickSort _ [] = []
myQuickSort a (x:xs) =
    (myAppend(myAppend(myQuickSort a left)[x])(myQuickSort a right))
    where (right, left) = myPartition(a x) xs