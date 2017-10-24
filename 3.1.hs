data WeirdPeanoNumber = Zero | Succ (WeirdPeanoNumber) | Pred (WeirdPeanoNumber) deriving Show

-- util
isSimple :: WeirdPeanoNumber -> Bool
isSimple Zero = True
isSimple (Succ (Pred _)) = False
isSimple (Pred (Succ _)) = True
isSimple (Succ num) = isSimple num
isSimple (Pred num) = isSimple num

simplify' :: WeirdPeanoNumber -> WeirdPeanoNumber
simplify' (Zero) = Zero
simplify' (Succ (Pred num)) = simplify' num
simplify' (Pred (Succ num)) = simplify' num
simplify' (Succ num) = Succ $ simplify' num
simplify' (Pred num) = Pred $ simplify' num

simplify :: WeirdPeanoNumber -> WeirdPeanoNumber
simplify num = let simplified = simplify' num in
                if (isSimple simplified) then simplified
                else simplify simplified

simpleEQ :: WeirdPeanoNumber -> WeirdPeanoNumber -> Bool
simpleEQ Zero Zero = True
simpleEQ Zero _ = False
simpleEQ _ Zero = False
simpleEQ (Succ lhv) (Succ rhv) = lhv `simpleEQ` rhv
simpleEQ (Succ lhv) (Pred rhv) = False
simpleEQ (Pred lhv) (Succ rhv) = False
simpleEQ (Pred lhv) (Pred rhv) = lhv `simpleEQ` rhv

simpleLEQ :: WeirdPeanoNumber -> WeirdPeanoNumber -> Bool
simpleLEQ Zero Zero = True
simpleLEQ Zero (Succ _) = True
simpleLEQ Zero (Pred _) = False
simpleLEQ (Succ _) Zero = False
simpleLEQ (Pred _) Zero = True
simpleLEQ (Succ lhv) (Succ rhv) = lhv `simpleLEQ` rhv
simpleLEQ (Succ lhv) (Pred rhv) = False
simpleLEQ (Pred lhv) (Succ rhv) = True
simpleLEQ (Pred lhv) (Pred rhv) = lhv `simpleLEQ` rhv

simpleDIV :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
simpleDIV lhv rhv = let dif = lhv - rhv in
                    if (dif >= Zero) then
                      (simpleDIV dif rhv) + 1
                    else 0


-- class types
instance Eq WeirdPeanoNumber where
  (==) lhv rhv = simpleEQ (simplify lhv) (simplify rhv)

instance Ord WeirdPeanoNumber where
  (<=) lhv rhv = simpleLEQ (simplify lhv) (simplify rhv)

instance Num WeirdPeanoNumber where
  (+) Zero rhv = rhv
  (+) lhv Zero = lhv
  (+) (Succ lhv) rhv = Succ (lhv + rhv)
  (+) (Pred lhv) rhv = Pred (lhv + rhv)

  negate Zero = Zero
  negate (Succ num) = Pred (negate num)
  negate (Pred num) = Succ (negate num) 

  fromInteger x | x == 0 = Zero
                | x < 0 = Pred (fromInteger (x + 1))
                | otherwise = Succ (fromInteger (x - 1))

  signum Zero = Zero
  signum (Succ (Pred num)) = signum num
  signum (Pred (Succ num)) = signum num
  signum (Succ num) = Succ Zero
  signum (Pred num) = Pred Zero

  abs num = if (signum num < Zero) then negate num else num

  (*) Zero _ = Zero
  (*) _ Zero = Zero
  (*) (Succ lhv) rhv = rhv + (lhv * rhv)
  (*) (Pred lhv) rhv = if (signum lhv == signum rhv) then  (rhv + (lhv * rhv))
                       else if (signum lhv < Zero) then negate(rhv + ((negate lhv) * rhv))
                       else let nrhv = negate rhv in negate (nrhv + (lhv * nrhv))

instance Enum WeirdPeanoNumber where
  toEnum num | num == 0 = Zero
             | num < 0 = Pred (toEnum $ num + 1)
             | otherwise = Succ (toEnum $ num - 1) 
  fromEnum Zero = 0
  fromEnum (Succ lhv) = (fromEnum lhv) + 1
  fromEnum (Pred lhv) = (fromEnum lhv) - 1

instance Real WeirdPeanoNumber where
   toRational num = toRational (toInteger num)

instance Integral WeirdPeanoNumber where
  quotRem lhv rhv = let isNeg = (signum lhv) == (signum rhv) in
                    let div = simpleDIV (abs lhv) (abs rhv) in
                    if (isNeg) then (div, simplify $ lhv - div * rhv) else (negate div, simplify $ lhv - div * rhv)

  toInteger Zero = 0
  toInteger (Succ lhv) = (toInteger lhv) + 1
  toInteger (Pred lhv) = (toInteger lhv) - 1


-- test functions
test :: String -> String
test str = show $ simplify (-5 * (Pred (Pred (Zero))))

main = interact test