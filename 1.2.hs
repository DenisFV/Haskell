import Data.Maybe

checkDate :: Int -> Int -> Int -> Bool
checkDate d m y = d>0 && m>0 && m<13 && y>=0 && d <=
    (if m `elem` [4,6,9,11] then 30
     else 
        if m == 2 then
            if (y `mod` 400 == 0) || (y `mod` 100 /= 0) && 
                (y `mod` 4 == 0) then 29
            else 28
        else 31)
 
nod :: Integer -> Integer -> Integer
nod a b | a == b = a
        | a > b = if a `mod` b == 0 then b else nod b (a - b)
        | otherwise = nod b a

job :: Int -> Int -> Maybe Int 
job l r = go $ floor (sqrt $ fromIntegral l :: Double) 
    where go i = case i*i of
                    k | k >= r -> Nothing
                      | k > l -> Just k
                    _ -> go $ i+1
