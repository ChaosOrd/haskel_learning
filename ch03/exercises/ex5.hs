isPalindrome [] = True
isPalindrome [a] = True
isPalindrome (x:xs) = x == (last xs) && (isPalindrome (init xs))

