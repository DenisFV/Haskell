data DList a = DEmpty | DCons (DList a) a (DList a)

rotateL::DList a->DList a
rotateL DEmpty = DEmpty
rotateL res@(DCons DEmpty _ _) = res
rotateL (DCons (DCons ll la lr) a r) = DCons ll la (DCons lr a r)

ins::DList a->Int->a->DList a
ins list n x = insL (rotateL list) n x where
                 insL DEmpty n x | n==0 = DCons DEmpty x DEmpty 
                                 | otherwise = error "Short list"
                 insL (DCons DEmpty a DEmpty) n x | n==0 = DCons (DCons DEmpty a DEmpty) x DEmpty
                                                  | n==1 = DCons (DCons DEmpty x DEmpty) a DEmpty
                                                  | otherwise = error "Short list" 
                 insL (DCons l a DEmpty) n x | n==0 = DCons l x (DCons DEmpty a DEmpty) 
                                             | n==1 = DCons l a (DCons DEmpty x DEmpty)
                                             | otherwise = error "Short list" 
                 insL (DCons DEmpty a r) n x | n==0 = DCons (DCons DEmpty a DEmpty) x r
                                             | n==1 = DCons (DCons DEmpty x DEmpty) a r
                                             | otherwise = error "Short list"
                 insL (DCons (DCons ll al _) a (DCons _ ar rr)) n x = if n<0 then error "Invalid argument" else newnode 
                  where newnode = DCons (DCons ll al newnode) a (DCons newnode ar (insL rr (n-1) x))
                              

del::DList a->Int->DList a
del list n = delL (rotateL list) n where 
               delL DEmpty _ = error "Short list"
               delL (DCons (DCons ll la lr) a (DCons rl ra rr)) 0 = newNode where
                 newNode = DCons ll la (DCons newNode ra rr)
               delL (DCons (DCons ll al _) a (DCons _ ar rr)) n = if n < 0 then error "Invalid argument" 
                 else newnode where newnode = DCons (DCons ll al newnode) a (DCons newnode ar (delL rr (n-1)))

ind::DList a->Int->a
ind list n = findL (rotateL list) n where findL DEmpty _ = error "Short list" 
                                          findL (DCons _ a _) 0 = a
                                          findL (DCons _ _ r) n | n < 0  = error "Invalid argument" 
                                                                | otherwise = findL r (n-1)
