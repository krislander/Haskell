main :: IO()
main = do

 print (solveQuadratic 2 8 2)
 print (sumPrimes 4 5)
 print (countPalindromes 100 112)
 print (truncatablePrime 3767)

-- Задача 1
solveQuadratic :: Double -> Double -> Double -> (Double, Double)
solveQuadratic a b c = if d<0 then error "0" else (x1, x2)
   where
     x1 = e + sqrt d / (2 * a)
     x2 = e - sqrt d / (2 * a)
     d = b * b - 4 * a * c
     e = - b / (2 * a)
     
-- Задача 2
sumPrimes:: Int -> Int -> Int
sumPrimes n 0 = 0
sumPrimes n x = 
      if prime n then n + sumPrimes (n+1) (x-1) else sumPrimes (n+1) x
    where
      prime :: Int -> Bool
      prime 1 = False
      prime n = null [d | d <- [2..(n-1)], n `mod` d == 0]


-- Задача 3
countPalindromes :: Int -> Int -> Int
countPalindromes a b = if a>b then 0
   else if isPalindrome a then 1 + countPalindromes (a+1) b else countPalindromes (a + 1) b
     where
       isPalindrome :: Int -> Bool
       isPalindrome n = if reverseNumber n == n then True else False
       reverseNumber :: Int -> Int
       reverseNumber n = helper n 0       
       helper :: Int -> Int -> Int
       helper x res = if x == 0 then res else helper (x `div` 10) (res * 10 + x `mod` 10)
       
       
-- Задача 4
truncatablePrime :: Int -> Bool
truncatablePrime 0 = True
truncatablePrime n = if prime n then truncatablePrime (n `div` 10) else False
   where
     prime :: Int -> Bool
     prime 1 = False
     prime n  = null [d | d <- [2..(n-1)], n `mod` d == 0]