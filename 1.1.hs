data Term = IntConstant{ intValue :: Int }    
            | Variable{ varName :: String }    
            | Add{ lAdd :: Term, rAdd :: Term } 
            | Sub{ lSub :: Term, rSub :: Term } 
            | Mul{ lMul :: Term, rMul :: Term } 
            | Neg{ neg :: Term } 
            deriving(Show,Eq)
 
(<+>),(<->),(<*>) :: Term -> Term -> Term
 
l <+> r = Add l r
l <-> r = Sub l r
l <*> r = Mul l r
 
infixl 6 <+>,<->
infixl 7 <*>
 
replaceVar :: String -> Int -> Term -> Term
replaceVar _ _ t@IntConstant{} = t 
replaceVar s v Variable{ varName = s' } | s==s' = IntConstant v
                                        | otherwise = Variable s'
replaceVar s v Add{lAdd=l,rAdd=r} = Add (replaceVar s v l) (replaceVar s v r)
replaceVar s v Sub{lSub=l,rSub=r} = Sub (replaceVar s v l) (replaceVar s v r)
replaceVar s v Mul{lMul=l,rMul=r} = Mul (replaceVar s v l) (replaceVar s v r)
replaceVar s v Neg{ neg=n} = Neg (replaceVar s v n)
 
