toDigits :: Integer -> [Integer]
toDigits n 
 | n < 1 = []
 | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]


getPerms = do
  a <- [0..9]
  b <- [a..9]
  c <- [b..9]
  d <- [c..9]
  e <- [d..9]
  f <- [e..9]
  g <- [f..9]
  return $ ( a * 10^6 ) + ( b * 10^5 ) + ( c * 10^4 ) + ( d * 10^3 ) + (e * 10^2 ) + (f * 10) + g


square x = x * x

squareDigitSum :: Integer -> Integer
squareDigitSum n = sum . map square $ toDigits n

squareDigitSumTerminus :: Integer -> Integer
squareDigitSumTerminus n
 | n == 0 = n
 | n == 1 = n
 | n == 89 = n
 | otherwise = squareDigitSumTerminus $ squareDigitSum n



squareDigitSumCount :: [Integer] -> Int
squareDigitSumCount xs = 
  let test x = x == 89
  in length . filter test $ map squareDigitSumTerminus xs
  


