disc :: Double -> Double -> Double -> Double
disc limit proc sum = max (sum * (1 - proc/100)) limit
discTen :: Double -> Double -> Double
discTen proc sum = disc 10 proc sum