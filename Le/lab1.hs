-- haskell function to convert a numeral to an integer (question 2 in lab)

digit :: Char -> Integer
digit '0' = 0
digit '1' = 1
digit '2' = 2
digit '3' = 3
digit '4' = 4
digit '5' = 5
digit '6' = 6
digit '7' = 7
digit '8' = 8
digit '9' = 9

numeralToInt :: [Char] -> Integer
numeralToInt ('-':xs) = numeralToInt xs *(-1)
numeralToInt (x:[]) = digit x
numeralToInt (x:xs) = digit x * (10^length xs) + numeralToInt xs
