main::IO()
main = do

 print(listLength [1,2,3,4,5,6])
 print(isElementOf 3 [1,2,4,5,6,7])
 print(interval 2 5)
 print(digits 1234)
 print(removeDuplicates [1, 2,2,2,3,3, 1])
 
{-
Задача 1. Да се дефинира функцията listLength xs, която приема списък xs и
връща неговата дължина (подобно на функцията length от стандартната Прелюдия).
-} 
listLength :: [a] -> Int
listLength xs = 
    if null xs
      then 0
      else 1 + listLength (tail xs)
      
{-
Задача 2. Да се дефинира функцията isElementOf x xs, която приема дадено число x
и списък от числа xs и връща дали x се съдържа в xs (подобно на функцията elem).
-}
isElementOf :: Integer -> [Integer] -> Bool
isElementOf x xs
     | null xs      = False
     | x == head xs = True
     | otherwise    = isElementOf x (tail xs) 
     
{-
Задача 3. Да се дефинира функция interval a b, която връща списък с числата в
интервала [a .. b] (за целта НЕ може да използвате израза [a .. b]).
-} 
interval :: Integer-> Integer-> [Integer]
interval a b = 
    if a>b then [] else a : interval (a+1) b
    
{-
Задача 4. Да се дефинира функция digits n, която връща списък с цифрите на
цялото число n >= 0.
-}
digits :: Integer->[Integer]
digits n 
    |n<0       = error "n < 0"
    |n<10      = [n]
    |otherwise = digits (n `div` 10) ++ [n `mod` 10]
    
{-
Задача 5. Да се дефинира функция removeDuplicates xs, която приема списък от
числа xs, и връща списък от числа, в който са премахнати всички дупликати в xs.
-}    
removeDuplicates :: [Integer] -> [Integer]
removeDuplicates xs = reverse (helper xs []) where
    helper items res
       | null items                   = res
       | isElementOf (head items) res = helper (tail items) res
       | otherwise                    = helper (tail items) (head items:res)
