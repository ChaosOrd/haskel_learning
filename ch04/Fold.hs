myFilter p xs = foldr step [] xs
    where step x ys | p x       = x : ys
                    | otherwise = ys

foldl' _ zero []        = zero
foldl' step zero (x:xs) = 
    let new = step zero x
    in new `seq` foldl' step new xs
