-- 1) returns the larges of a given list of values
myMaximum :: Ord a => [a] -> a
myMaximum lst =  if length lst == 1 then
                    head lst
                else if (head lst) > myMaximum (tail lst) then
                    head lst
                else
                    myMaximum (tail lst)

-- 2) takes a list and returns the reverse order of the list
myReverse :: Ord a => [a] -> [a]
myReverse lst =  if length lst == 0 then
                    []
                else 
                    (drop((length lst) - 1) lst) ++ myReverse (take ((length lst) - 1) lst)

-- 3) takes a value and a list and returns true if the value is inthe list, and false otherwise
myLength :: Ord a => [a] -> Int
myLength lst =   if length lst == 0 then
                    0
                else
                    1 + myLength (tail lst)

-- 4) takes two lists of values and returns true if all the valuesin the first list are in the second list. 
myElement :: Eq a => a -> [a] -> Bool
myElement a lst = if length lst == 0 
                then False
                else if (head lst) == a 
                then True
                else myElement a (tail lst)

-- 5) takes two lists of values and returns true if all the values in the first list are in the second list.                   
myElements :: Ord a => [a] -> [a] -> Bool
myElements xs ys =  if length xs == 0 then
                        True
                    else if myElement (head xs) ys then
                        myElements (tail xs) ys
                    else 
                        False

-- 6) takes a pair of values and a list and returns a new list suchthat each occurrence of 
--the first value of the pair in the list is replaced with the second value
myReplace :: Eq a => (a,a) -> [a] -> [a]
myReplace (x,y) xs =    if length xs == 0 then
                            xs
                        else if x == (head xs) then
                            myReplace (x,y) (take 0 xs ++ [y] ++ (drop (1) xs))
                        else
                            [(head xs)] ++ myReplace (x,y) (tail xs)

-- 7) takes a list of pairs and a list of values and returns a new list where each occurrence of the first 
--value in a pair is replaced by the second value in the pair.
myReplaceAll :: Eq a => [(a,a)] -> [a] -> [a]
myReplaceAll xs ys = if length xs == 0 then
                        ys
                    else myReplaceAll (tail xs) (myReplace (head xs) ys)

-- 8) takes a value and a list, and returns the sum of the givenvalues in the list.
myElementSum :: Int -> [Int] -> Int
myElementSum a xs = if length xs == 0 then
                        0
                    else if a == (head xs) then
                        a + myElementSum a (tail xs)
                    else
                        0 + myElementSum a (tail xs)

-- 9) takes  a  list  of  values,  and  returnsthe  original  list  with  duplicate  values  removed.
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates xs =   if length xs == 0 then
                            []
                        else if myElement (head xs) (tail xs) then
                            removeDuplicates (tail xs)
                        else
                            [(head xs)] ++ removeDuplicates (tail xs)

-- 10) takes  a  list  of  pairs  and  sorts  the  list  on  thefirst element of 
--the pair using the merge sort algorithm. 
mergeSort :: Ord a => [(a,b)] -> [(a,b)]
mergeSort xs = if null xs
                then []
                else if length xs == 1
                then xs
                else merge (mergeSort h1) (mergeSort h2)
                where 
                    h1 = take (div(length xs) 2) xs
                    h2 = drop (div(length xs) 2) xs

merge :: Ord a=> [(a,b)] -> [(a,b)] -> [(a,b)]
merge xs ys = if null ys 
                then xs
                else if null xs
                then ys
                else if fst (head xs) < fst (head ys) 
                then [(head xs)] ++ merge (tail xs) ys
                else [(head ys)] ++ merge xs (tail ys)