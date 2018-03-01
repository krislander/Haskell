main::IO()
main=do
 print "ex02_1&2.hs"
 print (countDigits 1234)
 print (sumDigits 1234)
 print (pow 4 5)
 print (sumDigitsIterative 103)
 print (reverseNumber 4567)
 print (isPrime 13)

countDigits :: Int -> Int
countDigits n = 
  if n<10 then 1 else 1 + countDigits (div n 10)

  
  
sumDigits :: Int -> Int
sumDigits n =
  if n<10 then n else (mod n 10) + sumDigits (div n 10)
 

pow :: Double -> Int -> Double
pow x n = 
 if n==0 then 1 else x * pow x (n-1)
 
 
sumDigitsIterative :: Int -> Int
sumDigitsIterative n = helper n 0
 where 
    helper :: Int -> Int -> Int
    helper k res =
      if k<10 then k + res else helper (div k 10) (res + mod k 10)
      
      
reverseNumber :: Int -> Int
reverseNumber n = helper n 0
  where
     helper :: Int -> Int -> Int
     helper k res = 
       if k==0 then res else helper (k `div` 10) (res*10 + k `mod` 10) 
       

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = helper 2
  where
    helper :: Int->Bool
    helper d
      |n == d         = True
      |n `mod` d == 0 = False
      |otherwise      = helper(d+1)
  