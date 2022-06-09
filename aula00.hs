answer :: Int
answer = 42

maxi :: Int -> Int -> Int
maxi m n
 |m>n=m
 |otherwise=n

maxi3 :: Int -> Int -> Int -> Int
maxi3 a b c = maxi a(maxi b c)
