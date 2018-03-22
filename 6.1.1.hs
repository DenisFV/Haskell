data DList a = DEmpty | DCons (DList a) a (DList a)

rotateL left DEmpty = DEmpty
rotateL left (DCons _ x r) = let rec = DCons left x (rotateL rec r) in rec

ins::Int->a->DList a->DList a
ins n | n <0 = error "invalid argument"
      | otherwise = go DEmpty n 
   where
      go left 0 v DEmpty  = DCons left v DEmpty
      go left _ v DEmpty = error "Short list"
      go left 0 v (DCons _ _ r) = let rec = DCons left v (rotateL rec r) in rec
      go left n v l@(DCons _ x r) = let rec = DCons left x (go rec (n-1) v r) in rec

del::Int->DList a->DList a
del n | n < 0 = error "invalid argument"
      | otherwise = go DEmpty n
   where
      go _ 0 DEmpty = DEmpty
      go _ _ DEmpty = error "short list"
      go left 0 (DCons _ _ r) = let rec = (rotateL left r) in rec     
      go left n (DCons _ x r) = let rec = DCons left x (go rec (n-1) r) in rec
      
ind::Int->DList a->Maybe a
ind _ DEmpty = Nothing
ind 0 (DCons _ x _) = Just x
ind n (DCons _ _ r) = ind (n-1) r