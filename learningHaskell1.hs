-- 1) returns the middle value of three different values
median3 :: (Num a, Ord a) => a -> a -> a -> a
median3 x y z =
    if x == y || x == z || x > y && x < z || x < y && x > z then
        x
    else if y > x && y < z || y < x && y > z then
        y
    else
        z

-- 2) returns the midpoint between two points
midpoint :: (Fractional a, Fractional b) => (a,b) -> (a,b) -> (a,b)
midpoint (x,y) (x2,y2) =
    ((x + x2) / 2, (y + y2) / 2)

-- 3) takes a list and returns a list containing all of the elements(in the same order) 
--    except for the last element
allButLast :: [a] -> [a]
allButLast xs =
    if not (length xs == 0) then
        take ((length xs) - 1) xs
    else
        error "Invalid List: Empty List"

-- 4) takes a list and returns last element
lastElem :: [a] -> a
lastElem xs =
    if not (length xs == 0) then
        head (drop ((length xs) - 1) xs)
    else
        error "Invalid List: Empty List"

-- 5) returns the element in the list at a given list position
elemAt :: Int -> [a] -> a
elemAt i (x:xs) =
    if i <= 0 then
        x
    else
        elemAt (i-1) xs

-- 6) replaces thei-th element in a list with a given value
replaceInList :: [a] -> Int -> a -> [a]
replaceInList xs i x =
    if i < (length xs) || i < 0 then
        take (i) xs ++ [x] ++ (drop 1 xs)
    else
        error "Invalid index: Out of range"

-- 7) takes a list of values of sizenand returns a list containingn
--    of the largest values in the given lis
replaceMax ::  Ord a => [a] -> [a]
replaceMax xs =
    lstMax
    where
        lstMax = take (length xs) (repeat (maximum xs))

-- 8) takes  two  lists  of  values  and  returnsthe  pairwise  elements  where  the  
--    first  element  is  strictly  larger  than  the  sec-ond  element.
largeToSmallPairs :: Ord a => [a] -> [a] -> [(a,a)]
largeToSmallPairs xs ys =
    if not (length xs == 0) && not (length ys == 0) then
        let x = head xs
            y = head ys
    in if x > y then 
        [(x,y)] ++ largeToSmallPairs (tail xs) (tail ys)
        else
            largeToSmallPairs (tail xs) (tail ys)
    else []

-- 9) takes  two  lists  of  values  and  returnsthe  pairwise  elements  
--    where  the  first  element  is  strictly  larger  than  the  sec-ond  element.
containsElem :: Eq x => x -> [x] -> Bool
containsElem v xs =
    if length xs == 0 then
        False
    else if (head xs) == v then
        True
    else 
        containsElem v (tail xs)

-- 10) finds elements in one list of pairs matching a given valueand combines 
--     them with another list of pairs also matching a given value. 
combine :: Eq a => a -> [(a,b)] -> [(a,b)] -> [(a,b)]
combine a xs ys = 
    let com = xs ++ ys
    in filter (\y -> fst y == a) com
