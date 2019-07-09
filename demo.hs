sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = sc (abs x) 0 0 
    where
        sc x sum count | x < 10 = (sum + x, count + 1)
                       | otherwise = sc (quot x 10) (sum + (mod x 10)) (count + 1)

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = 
    let fa = f a
        fb = f b
        square a b = (fa * fb) - (fa - fb)
    in square a b

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)
    