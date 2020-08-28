-- 1) returns the larges of a given list of values
myMaximum :: Ord a => [a] -> a
myMaximum (x:xs)  
        | length (x:xs) == 1 = x
        | x > myMaximum xs = x
        | otherwise = myMaximum xs

-- 2) takes a list and returns the reverse order of the list
myReverse :: Ord a => [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (drop((length (x:xs)) - 1) (x:xs)) ++ myReverse (take ((length (x:xs)) - 1) (x:xs))

-- 3) takes a value and a list and returns true if the value is inthe list, and false otherwise
myLength :: Ord a => [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- 4) takes two lists of values and returns true if all the valuesin the first list are in the second list. 
myElement :: Ord a => a -> [a] -> Bool
myElement _ [] = False
myElement a (x:xs)
        | x == a = True
        | otherwise = myElement a xs

-- 5) takes two lists of values and returns true if all the values in the first list are in the second list.        
myElements :: Ord a => [a] -> [a] -> Bool
myElements [] _ = True
myElements (x:xs) ys
        |   myElement x ys = myElements xs ys
        |   otherwise = False

-- 6) takes a pair of values and a list and returns a new list suchthat each occurrence of 
--the first value of the pair in the list is replaced with the second value
myReplace :: Eq a => (a,a) -> [a] -> [a]
myReplace rep [] = []
myReplace (a,b) (x:xs)
        | a == x = [b] ++ myReplace (a,b) xs
        | otherwise = [x] ++ myReplace (a,b) xs

-- 7) takes a list of pairs and a list of values and returns a new list where each occurrence of the first 
--value in a pair is replaced by the second value in the pair.
myReplaceAll :: Eq a => [(a,a)] -> [a] -> [a]
myReplaceAll [] lst = lst
myReplaceAll (x:xs) lst = myReplaceAll xs (myReplace x lst)

-- 8) takes a value and a list, and returns the sum of the givenvalues in the list.
myElementSum :: Int -> [Int] -> Int
myElementSum _ [] = 0
myElementSum a (x:xs)
        | a == x = a + myElementSum a xs
        | otherwise = 0 + myElementSum a xs

-- 9) takes  a  list  of  values,  and  returnsthe  original  list  with  duplicate  values  removed.
removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
        | myElement x xs = removeDuplicates xs
        | otherwise = [x] ++ removeDuplicates xs

mergeSort [] = []
mergeSort [x] = [x]
mergeSort lst = merge (mergeSort firstHalf) (mergeSort secondHalf)
        where
                firstHalf = take (div (length lst) 2) lst
                secondHalf = drop (div (length lst) 2) lst

-- mergeSort helper function                
merge :: Ord a => [(a,b)] -> [(a,b)] -> [(a,b)]
merge lst1 [] = lst1
merge [] lst2 = lst2
merge (x:xs) (y:ys)
        | fst x < fst y = [x] ++ merge xs (y:ys)
        | otherwise = [y] ++ merge (x:xs) ys