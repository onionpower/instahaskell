integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = 
    let
        acc = 0        
        (l, r, sign) = if a < b then (a, b, 1) else (b, a, -1)
        inc = (r - l)/1000
    in sign*inc*((f l + f r)/2 + simit acc inc (l + inc) (r - inc) f)

simit :: Double -> Double -> Double -> Double -> (Double -> Double) -> Double
simit acc inc l r f
        | l > r = acc
        | otherwise = simit (acc + f l) inc (l + inc) r f
        where
            it f l = f l
