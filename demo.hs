doubleFact :: Integer -> Integer
doubleFact n | n < 2 = 1
             | otherwise = n * doubleFact (n - 2)