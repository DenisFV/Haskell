data BinaryTree = EmptyTree
                | Limb { left::BinaryTree, value::Integer, right::BinaryTree, parent::BinaryTree }
                
instance Eq BinaryTree where
  EmptyTree == EmptyTree = True
  (Limb lh _ lt _) == (Limb rh _ rt _) = (lh == rh) && (lt == rt)
{-
--отображение дерева ввиде списка
instance Show BinaryTree where
  show lst = "[" ++ show' lst ++ "]" where show' EmptyTree = ""
                                           show' (Limb EmptyTree h EmptyTree _) = show h
                                           show' (Limb l h EmptyTree _) = show' l ++ ", " ++ show h
                                           show' (Limb EmptyTree h r _) = show h ++ ", " ++ show' r
                                           show' (Limb l h r _) = show' l ++ ", " ++ show h ++ ", " ++ show' r
-}
--отображение дерева ввиде дерева
instance Show BinaryTree where
  show lst = show' lst
    where 
     show' EmptyTree = "EmptyTree"
     show' (Limb EmptyTree h EmptyTree _) = "Limb EmptyTree "++ show h ++ " EmptyTree"
     show' (Limb l h EmptyTree _) = "Limb ("++show' l ++") "++ show h ++" EmptyTree"
     show' (Limb EmptyTree h r _) = "Limb EmptyTree "++show h ++" ("++ show' r++")"
     show' (Limb l h r _) = "Limb ("++show' l ++") "++ show h ++" ("++ show' r ++")"                                           
                                           
                                           
g  = fromList [1..10]

--Создание дерева с 1 элементом
singleton :: Integer -> BinaryTree
singleton a = Limb EmptyTree a EmptyTree EmptyTree

--Создание дерева из списка
fromList :: [Integer] -> BinaryTree
fromList x = let kor = (!!) x (div (length x) 2)
                 rez = (filter (<kor) x)++(filter (>kor) x)
             in go (singleton kor) rez 
 where
  go l []       = l
  go l q@(x:xs) = go (insert x l) xs

--превращение дерева в список
uncov :: BinaryTree -> [Integer]
uncov EmptyTree = []
uncov (Limb l v r _) = (uncov l)++v:(uncov r)

--слияние 2 деревьев 
merge l r = case (length $ uncov l) `compare` (length $ uncov r) of
   LT -> merge' r l
   _  -> merge' l r

merge' l r = go l (uncov r)
 where 
  go l [] = l
  go l (x:xs) = let a' = (insert x l) in seq a' (go a' xs)  


--балансировка дерева
balance :: BinaryTree -> BinaryTree
balance l = fromList $ uncov l 

--корень дерева
koren::BinaryTree->Integer
koren EmptyTree = 0
koren (Limb _ x _ _) = x 

--поиск веса дерева
height::BinaryTree->Integer
height EmptyTree = 0
height (Limb l x r _) = max (1+(height l)) (1+(height r))

--количетсво всех узлов вдереве
size::BinaryTree->Integer
size EmptyTree = 0
size (Limb l x r _) = 1 + (height l) + (height r)


           
--проверка наличия значения в дереве
index :: Integer -> BinaryTree -> Maybe Integer
index _ EmptyTree = Nothing
index n (Limb l v r _) | n==v = Just v
                       | n<v = index n l
                       | otherwise = index n r

--вывод ветви дерева от корня до значения
index2 _ EmptyTree = EmptyTree
index2 n q@(Limb l v r _) | n==v      = q
                          | n<v       = index2 n l
                          | otherwise = index2 n r
   


--вставка значения в дерево
insert::Integer->BinaryTree->BinaryTree
insert = go EmptyTree
 where
  go p x EmptyTree = Limb EmptyTree x EmptyTree p
  go p x (Limb l e r _) | x<e       = let rec = Limb (go rec x l) e (newParent rec r) p in rec
                        | otherwise = let rec = Limb (newParent rec l) e (go rec x r) p in rec
newParent = go
 where
  go parent EmptyTree = EmptyTree
  go parent (Limb l v r _) = let rec = Limb (newParent rec l) v (newParent rec r) parent in rec

--более быстрая вставка
ins n l = fromList $ n:uncov l 


--удаление всех вхождений значения в дерево
delete :: Integer -> BinaryTree -> BinaryTree
delete x l | index x r == Nothing = r
           | otherwise = delete x r
           where r = delete' x l                        

--альтернативное удаление всех вхождений значения в дерево
del n l = fromList $ filter (/=n) (uncov l)

--удаление 1-ого вхождения значения из дерева
delete' = go EmptyTree
 where
  go p x EmptyTree = EmptyTree
  go p x (Limb l e r _) | x<e  = let rec = Limb (go rec x l) e (newParent rec r) p in rec
                        | x>e  = let rec = Limb (newParent rec l) e (go rec x r) p in rec
                        | otherwise = pmerge p l r

pmerge p l r = go p l (uncov r)
 where 
  go _ l []     = l
  go p l (x:xs) = seq a' (go p a' xs) where a' = (insert' p x l)
 
insert' = go 
 where
  go p x EmptyTree = Limb EmptyTree x EmptyTree p
  go p x (Limb l e r _) | x<e       = let rec = Limb (go rec x l) e (newParent rec r) p in rec
                        | otherwise = let rec = Limb (newParent rec l) e (go rec x r) p in rec   