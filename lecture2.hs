main::IO()
main=do

 print (countDigits 1234)
 print (sumDigits 1234)
 print (pow 4 5)
 print (sumDigitsIterative 103)
 print (reverseNumber 4567)
 print (isPrime 13)
 print (isAscending 12343)
 print (countOccurences 5 556)
 print (isPerfectNumber 6)

--Зад. 1. Да се дефинира функция countDigits, която генерира 
--линейно рекурсивен процес и намира броя на цифрите на дадено 
--естествено число.
countDigits :: Int -> Int
countDigits n = 
  if n<10 then 1 else 1 + countDigits (div n 10)
  
{-
  Зад. 2. Да се дефинира функция sumDigits, която генерира линейно рекурсивен 
  процес и намира сумата от цифрите на дадено естествено число.
-}  
sumDigits :: Int -> Int
sumDigits n =
  if n<10 then n else (mod n 10) + sumDigits (div n 10)
 
{-
  Зад. 3. Да се дефинира функция pow, която генерира линейно рекурсивен процес 
  и намира x на степен n, където x е реално, а n - естествено число.
-}
pow :: Double -> Int -> Double
pow x n = 
 if n==0 then 1 else x * pow x (n-1)
 
{-
  Зад. 4. Да се дефинира функция sumDigitsIterative, която генерира линейно 
  итеративен процес и намира сумата от цифрите на дадено естествено число.
-} 
sumDigitsIterative :: Int -> Int
sumDigitsIterative n = helper n 0
 where 
    helper :: Int -> Int -> Int
    helper k res =
      if k<10 then k + res else helper (div k 10) (res + mod k 10)
      
{-
  Зад. 5. Да се дефинира функция reverseNumber, която генерира линейно итеративен
  процес и по дадено естествено число n намира числото, записано със същите цифри,
  но в обратен ред.
-}      
reverseNumber :: Int -> Int
reverseNumber n = helper n 0
  where
     helper :: Int -> Int -> Int
     helper k res = 
       if k==0 then res else helper (k `div` 10) (res*10 + k `mod` 10) 
       
{-
  Зад. 6. Да се дефинира предикат isPrime, който проверява дали дадено естествено
  число е просто.
  Забележка: Числото 1 не е нито просто, нито съставно.
-}
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
      
{-
  Зад. 7. Да се напише предикат isAscending, който връща истина, ако цифрите на
  дадено естествено число са в нарастващ ред от първата към последната.
-}      
isAscending :: Int -> Bool
isAscending n
   |n<10 = True
   |(n `mod` 10) < (n `div` 10) `mod` 10 = False
   |otherwise = isAscending (n `div` 10)
   
{-
  Зад. 8. Да се напише функция countOccurences, намираща броя на срещанията на дадена
  цифра d в записа на число n.
-}
countOccurences :: Int -> Int -> Int
countOccurences d n = 
  if d == n `mod` 10 then 1 else 1 + countOccurences d (n `div` 10)


{-
  Зад. 9. Да се напише предикат isPerfectNumber, който връща дали едно число е
  съвършено, т.е. равно на сумата от делите си.
-}
isPerfectNumber :: Int -> Bool
isPerfectNumber num = num == sumDivisors 1 0 where
    sumDivisors i res
      | i>=num           =res
      | num `mod` i == 0 = sumDivisors (i+1) (res+i)
      | otherwise        = sumDivisors (i+1) res
