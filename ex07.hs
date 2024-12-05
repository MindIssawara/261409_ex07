-- write this function in point-free style:
contains1 :: (Foldable t, Ord a) => a -> t a -> Bool
contains1 = \x l -> any (x<) l
-- hint: any :: (a -> Bool) -> [a] -> Bool
--      \x l -> any (x<) l
--      \x l -> (any.(x<)) l
--      \x -> any.(x<)
--      \x -> (.) any (x<)
--      (.) any (<)
contains1' :: (Foldable t, Ord a) => a -> t a -> Bool
contains1' = (.) any (<)

-- write this function in point-free style:
contains2 :: (Foldable t, Ord a) => t a -> a -> Bool
contains2 = \l x -> any (x<) l
-- hint: use flip function
-- Note :   flip :: (a -> b -> c) -> (b -> a -> c)
--          flip f x y = f y x

--      contains2 = flip (\x l -> any (x <) l)
--      contains2 = flip (contains1)
--      contains2 = flip ((.) any (<))
contains2' :: (Foldable t, Ord a) => t a -> a -> Bool
contains2' = flip ((.) any (<))

-- write function len_comp that uses list comprehension to compute the length of the given list
len_comp :: Num a => [t] -> a
len_comp ls = sum[1| x <- ls]

-- rewrite [(x,y) | x <- [2,3,5], y <- [1,2,4], even $ x+y] without using list comprehension
concat_list :: [[a]] -> [a]
concat_list [] = []
concat_list (x:xs) = x ++ concat_list xs

rewrite :: Integral b => [(b, b)]
rewrite = concat_list (map(\x -> map (\y -> (x, y)) $ filter (\y -> even (x + y)) [1,2,4] ) [2,3,5])
-- [(2,2),(2,4),(3,1),(5,1)]
