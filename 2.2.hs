-- folds
my_foldr :: (a -> b -> b) -> b -> [a] -> b
my_foldr f x [] = x
my_foldr f x (h : t) = f h (my_foldr f x t)

my_foldl :: (b -> a -> b) -> b -> [a] -> b
my_foldl f x [] = x
my_foldl f x (h : t) = my_foldl f (f x h) t

-- list functions
-- map
my_map :: (a -> b) -> [a] -> [b]
my_map _ [] = []
my_map f list = my_foldr (\h t -> (f h):t) [] list

-- flatMap
my_flatMap :: (a -> [b]) -> [a] -> [b]
my_flatMap _ [] = []
my_flatMap f list = my_foldr (\h t -> (f h) ++ t) [] list

-- concat
my_concat :: [a] -> [a] -> [a]
my_concat [] [] = []
my_concat list1 list2 = my_foldr (\h t -> h:t) list2 list1

-- filter
my_filter :: (a -> Bool) -> [a] -> [a]
my_filter _ [] = []
my_filter f list = my_foldr (\h t -> if (f h) then (h:t) else t) [] list

-- maxBy
my_maxBy :: (a -> Integer) -> [a] -> a
my_maxBy f list = let hl = head list in 
                  my_foldr (\h max -> if (f h) > (f max) then h else max) hl list

-- minBy
my_minBy :: (a -> Integer) -> [a] -> a
my_minBy f list = let hl = head list in 
                  my_foldl (\min h -> if (f h) < (f min) then h else min) hl list

-- reverse
my_reverse :: [a] -> [a]
my_reverse [] = []
my_reverse list = my_foldl (\t h -> h:t) [] list

-- elementAt
my_elementAt :: Integer -> [a] -> a
my_elementAt index (h:t) = res
                           where (_, res) = my_foldl (\ind_pair h -> let (cur_ind, value) = ind_pair in
                                                                        if (cur_ind < index) then (cur_ind + 1, value)
                                                                        else if (cur_ind == index) then (cur_ind + 1, h)
                                                                        else (cur_ind, value)) (0, h) (h:t)

-- indexOf
my_indexOf :: String -> [String] -> Integer
my_indexOf str list = let pr = my_foldl (\ind_pair h -> let (index, flag) = ind_pair in 
                                                        if (not flag) then if (h == str) then (index, True) else (index + 1, False)
                                                        else ind_pair) (0, False) list in
                                                        let (index, _) = pr in
                                                        index

-- test functions
test :: String -> String
test str = show $ my_filter (\x -> if (x > 1) then True else False) [0, 1, 2, 3]

main = interact test