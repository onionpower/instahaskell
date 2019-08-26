integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = 
    let
        acc = 0        
        (l, r, sign) = if a < b then (a, b, 1) else (b, a, -1)
        inc = (r - l)/1000
    in (sign * simit acc inc l (l + inc) f r) / 2

simit :: Double -> Double -> Double -> Double -> (Double -> Double) -> Double -> Double
simit acc inc l r f b
        | r > b = acc
        | otherwise = simit (acc + trapsqr f l r) inc (l + inc) (r + inc) f b
        where
            trapsqr f l r = (r - l) * (f l + f r)
