mySum :: Num t => [t] -> t
mySum (x:xs) = x + (mySum xs)
mySum [] = 0

myLength :: Num t => [t] -> t
myLength (x:xs) = 1 + (myLength xs)
myLength [] = 0

mean :: (Integral a, Fractional a) => [a] -> a
mean xs = (mySum xs) / (fromIntegral(myLength xs))
