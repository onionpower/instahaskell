sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x == 0 = (0, 1)
              | x < 0 = sc (abs x) 0 0
              | otherwise = sc x 0 0
sc :: Integer -> Integer -> Integer -> (Integer, Integer)
sc x sum count | x > 0 = sc (quot x 10) (sum + (mod x 10)) (count + 1)
               | otherwise = (sum, count)

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = 
    let fa = f a
        fb = f b
        square a b = (fa * fb) - (fa - fb)
    in square a b

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)
    