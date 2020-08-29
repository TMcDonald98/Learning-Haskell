data List a = Node a (List a)
        | Nil 
        deriving (Show, Eq)

-- 1)  returns a new list with the value inserted at the end of the list
insert :: a -> List a -> List a
insert x (Node y Nil) = Node y (Node x Nil)
insert x (Node y tail) = Node y (insert x tail)

-- 2)returns a newlist with the first occurence of the value removed from the list
delete :: Eq a => a -> List a -> List a
delete _ Nil = Nil
delete x (Node y tail)
        | x == y = delete x tail
        | otherwise = Node y (delete x tail)

-- 3) returns true if the given value is in the given list.
memberOf :: Eq a => a -> List a -> Bool
memberOf _ Nil = False
memberOf x (Node y tail)
        | x == y = True
        | otherwise = memberOf x tail

-- 4) returns the i-th element in the list.
elementAt :: Int -> List a -> a
elementAt _ Nil =  error "index out of range"
elementAt a (Node y tail)
        | a == 0 = y
        | otherwise = elementAt (a - 1) tail

-- 5)  returns the list with the value inserted into the i-th position of the list
insertAt :: Int -> a -> List a -> List a
insertAt 0 x Nil = Node x Nil
insertAt _ x Nil = error "index out of range"
insertAt 0 x (Node y Nil) = Node x (Node y Nil)
insertAt 0 x (Node y tail) = Node x (Node y (insertAt (-1) x tail))
insertAt a x (Node y tail) = Node y (insertAt (a - 1) x tail)

-- 6) returns thelist with the i-th element removed
deleteAt :: Int -> List a -> List a
deleteAt 0 (Node y Nil) = Nil
deleteAt _ (Node y Nil) = Node y Nil
deleteAt a (Node y tail)
        | a == 0 = deleteAt (a - 1) tail
        | otherwise = Node y (deleteAt (a - 1) tail)

-- 7) returns the result of concatenatingthe first list with the second list.
concatenate :: List a -> List a -> List a
concatenate (Node y Nil) (Node z ztail) = Node y (Node z (concatenate Nil ztail))
concatenate Nil (Node z ztail) = Node z (concatenate Nil ztail)
concatenate Nil Nil = Nil
concatenate (Node y ytail) (Node z ztail) = Node y (concatenate ytail (Node z ztail))

-- 8) returns a sorted (from smallest to largest value) of the original list
sortList :: Ord a => List a -> List a
sortList Nil = Nil
sortList (Node y tail)
        | tail == Nil = Node y Nil
        | y <= comp tail = Node y (sortList tail)
        | otherwise = Node (comp tail) (Node y (sortList (advance tail)))

comp :: Ord a => List a -> a
comp (Node y tail) = y

advance :: List a -> List a
advance Nil = Nil
advance (Node y tail) = tail

-- 9) mimics the map function on a list
mapList :: (a -> b) -> List a -> List b
mapList _ Nil = Nil
mapList fun (Node y tail) = Node (fun y) (mapList fun tail)

-- 10) mimics  the zip With function on the two given lists.                
zipWithList :: (a -> b -> c) -> List a -> List b -> List c
zipWithList _ Nil _ = Nil
zipWithList _ _ Nil = Nil
zipWithList fun (Node y ytail) (Node z ztail) = Node (fun y z) (zipWithList fun ytail ztail)