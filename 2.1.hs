data BinaryTree = Leaf | Branch Integer BinaryTree BinaryTree
                                   deriving (Show)
 
emptyTree :: BinaryTree
emptyTree = Leaf
 
containsElement :: BinaryTree -> Integer -> Bool
containsElement Leaf _ = False
containsElement (Branch key left right) k
    | k < key  = containsElement left k
    | k > key  = containsElement right k
    | otherwise = True
 
nearestGE :: BinaryTree -> Integer -> Integer
nearestGE t k = get t 0
    where get Leaf p = p
          get (Branch key left right) p
            | k < key  = get left  key
            | k > key  = get right p
            | otherwise = k
 
insert :: BinaryTree -> Integer -> BinaryTree
insert Leaf k = Branch k Leaf Leaf
insert (Branch key left right) k
    | k < key  = Branch key (insert left k) right
    | k > key  = Branch key left (insert right k)
    | otherwise = Branch key left right
 
remove :: BinaryTree -> Integer -> BinaryTree
remove Leaf _ = Leaf
remove (Branch key left right) k
    | k < key  = Branch key (remove left k) right
    | k > key  = Branch key left (remove right k)
    | otherwise = if isLeaf right
                 then left
                 else Branch leftmost left right'
                     where
                       isLeaf Leaf = True
                       isLeaf _    = False
                       (leftmost, right') = deleteLeftmost right
                       deleteLeftmost (Branch key'' Leaf right'') = (key'', right'')
                       deleteLeftmost ~(Branch key'' left'' right'') = (pair, Branch key'' left' right'')
                            where (pair, left') = deleteLeftmost left''
 
treeFromList :: [Integer] -> BinaryTree
treeFromList = foldr (flip insert) Leaf
 
listFromTree :: BinaryTree -> [Integer]
listFromTree tree = toList' tree []
    where
      toList' Leaf list = list
      toList' (Branch key left right) list = toList' left (key: toList' right list)
