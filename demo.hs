single :: Char -> Char -> Int
single x y | isDigit x && isDigit y = digitToInt x * 10 + digitToInt y
           | otherwise = 100