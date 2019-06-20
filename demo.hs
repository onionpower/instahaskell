--a0 = 1, a1 = 2, a2 = 3, a(k+3) = a(k+2) + a(k+1) - 2*ak

seqA :: Integer -> Integer
seqA 0 = 1
seqA 1 = 2
seqA 2 = 3
seqA n = seqI 3 2 1 4 n

seqI :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
seqI n2 n1 n it max | it > max = n2 + n1 + 2*n
                    | True = seqI (n2 + n1 + 2*n) n2 n1 (it + 1) max

seqB :: Integer -> Integer
seqB 0 = 1
seqB 1 = 2
seqB 2 = 3
seqB n =  2 * seqB (n - 3) + seqB (n - 2) + seqB (n - 1)