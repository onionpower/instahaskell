sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x < 0 = sc (abs x) 0 0
              | otherwise = sc x 0 0
sc :: Integer -> Integer -> Integer -> (Integer, Integer)
sc x sum count | x > 0 = sc (quot x 10) (sum + (mod x 10)) (count + 1)
               | otherwise = (sum, count)