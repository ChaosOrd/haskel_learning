toPalindrome (x:xs) = [x] ++ (toPalindrome xs) ++ [x]
toPalindrome [] = []
