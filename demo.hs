fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n | n > 0 = incfib 0 1 0 (n - 1)
      | n < 0 = decfib 0 1 0 (n * (-1) - 1)

incfib :: Integer -> Integer -> Integer -> Integer -> Integer
incfib cur next it max 
                    | it > max = cur
                    | True = incfib (next) (cur + next) (it + 1) max

decfib :: Integer -> Integer -> Integer -> Integer -> Integer
decfib cur next it max
                    | it > max = cur * (-1)^max
                    | True = decfib (next) (cur + next) (it + 1) max
inc n = incI 0 0 n
incI acc it n | it > n = acc
              | True = incI (acc + it) (it + 1) n