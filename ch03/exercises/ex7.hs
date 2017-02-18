listJoin []     = ""
listJoin (x:[]) = x
listJoin (x:xs) = x ++ "," ++ listJoin(xs)
