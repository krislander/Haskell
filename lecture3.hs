main::IO()
main = do

 print(12 ## 2)
 print(isNarcissistic 153)
 print(taylorLog 100 0.5, log 1.5)
--print(isAutomorphic 6)
 print(calcSum 3 2)
 print(sumPrimeDivisors 12)
 
{-
  Зад. 1. Напишете оператор n ## k, който приема n > 0 и k >= 0 и връща сумата
  от всяка цифра на n повдигната на степен k.
-}
(##) :: Integer -> Integer -> Integer
n ## k 
   | n<0       = error "n<0"
   | n == 0    =0
   | otherwise = (n `mod` 10)^k + (n `div` 10) ## k
   
   
{-
  Зад. 2. Да се дефинира функцията isNarcissistic n, която приема като аргумент
  цялото положително число n и връща дали то е нарцистично. Нарцистични се наричат
  числата, които са равни на сбора на цифрите си (в десетична бройна система),
  всяка повдигната на степен броя на цифрите на числото.
-}
countDigits :: Integer -> Integer
countDigits n = if n < 10 then 1 else 1+countDigits (n `div` 10)

isNarcissistic :: Integer -> Bool
isNarcissistic n = n == n ## k where
   k = countDigits n
   
{-
Задача 3. Серия на Тейлър: дефинирайте функцията taylorLog n x, която примема целочисления аргумент n и
реалното число x и връща сумата от първите n члена от реда на Тейлър на функцията ln(1 + x), дефиниран като:

ln(1 + x) = Sum{k = 0 ... n} (- 1) ^ k * x ^ (k + 1) / (k + 1)
-}
taylorLog :: Int -> Double -> Double
taylorLog n x
    | n < 0    = error "n >= 0"
    | n == 0   = x
    |otherwise = (-1)^n*x^(n+1)/fromIntegral (n+1) + taylorLog (n-1) x

{-
  Зад. 4. Да се дефинира предикат isAutomorphic, който приема число n и
  проверява дали n^2 завършва с цифрите на n.


isAutomorphic :: Integer -> Bool
isAutomorphic n = 
   if n == n^2 `mod` 10^CountDigits n then True else False
   -}
   
{-
  Зад. 5. По зададени x и n, да се изчисли сумата: 1 + x + x^2 + x^3 + ... + x^n.
-}
calcSum :: Int -> Int -> Int
calcSum x n = helper 0 where
   helper i
     |i>n       = 0
     |x==0      = 1
     |otherwise = x^i + helper(i+1)
       
       
{-
  Зад. 6. Да се дефинира функция sumPrimeDivisors, която намира сумата на всички
  прости делители на едно число.
  Tuk ima i funkciq za opredelqne dali edno chislo e prosto!!!!
-}
isPrime :: Integer -> Bool
isPrime n = n >= 2 && helper 2 where
    helper i
        | i > sqrtn         = True
        | n `mod` i == 0    = False
        | otherwise         = helper (i + 1) where
            sqrtn = floor (sqrt (fromIntegral n))
            
sumPrimeDivisors :: Integer -> Integer
sumPrimeDivisors n = helper 2 where
    helper i
      | i > n                       = 0
      | n `mod` i == 0 && isPrime i = i + helper (i+1)
      | otherwise                   = helper (i+1)       
   
   