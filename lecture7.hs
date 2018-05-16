import Data.Char
main::IO()
main=do

 print(enumerate "abcdefghijklmn")
 print(allCapsAtOddPos "kKaKa PaPa CiC")
 print(anyDigitAtEvenPos "q12W2")
 print(indices 1 [1, 2, 3, 1, 4])
 print(primeReorder [2,3,4,5,6])
 print(lastIndex 1 [1, 2, 7, 1, 5, 4])
 
{-
Задача 1. Дефинирайте функцията enumerate xs, коят прима списък xs и връща списък от
двойки (i, x), където i e индекса на x в xs.
-}
enumerate :: Integral b => [a] -> [(b, a)]
enumerate xs = zip [0..] xs 

{-
Задача 2.
а). Дефинирайте функцията allCapsAtOddPos str, която приема символен низ str и връща дали
    всички символи на нечетна позиция са главни букви.

б). Дефинирайте функцията anyDigitAtEvenPos str, която приема символен низ str и връща дали
    някой от символите на четна позиция е цифра.
-}

allCapsAtOddPos :: String -> Bool
allCapsAtOddPos str = and [isUpper c | (i,c) <- enumerate str, odd i]

anyDigitAtEvenPos :: String -> Bool
anyDigitAtEvenPos str = or [isDigit n | (i,n) <- enumerate str, even i]
 
 
{-
Задача 3. Дефинирайте функцията indices x xs, която връща всички индекси на елементи
от списъка xs, чиято стойност е равна на x.
    indices 1 [1, 2, 3, 1, 4] = [0, 3]
    indices 1 [] = []
-} 
indices :: Eq a => a -> [a] -> [Int]
indices x xs = [i | (i, y) <- enumerate xs, y == x]


{-
Задача 4. Просто пренареждане: Дефинирайте функцията primeReorder xs, която получава
списък xs и връща нов списък ys. В началото на ys трябва да са елементите, които са били
с индекс просто число в xs. След тях трябва да са всички останали. Индексирането в xs,
започва от 2.
-}
isPrime :: Integer -> Bool
isPrime n = null [ d | d <- [2..(n-1)], n `mod` d == 0]

primeReorder :: [a] -> [a]
primeReorder xs = helper isPrime ++ helper (not.isPrime) where
    helper p = [x | (i,x) <-zip [2..] xs, p i]
    
    
{-
Задача 5: Дефинирайте функцията lastIndex x xs, която приема 2 аргумента - списък от числа и
число и връща индексa (0-базиран) на последното срещане на числото в списъка. Ако числото не
се среща в списъка функцията връща грешка.
    lastIndex 1 [1, 2, 7, 1, 5, 4] = 3
-}    

lastIndex :: Integer->[Integer] -> Integer
lastIndex n xs
    | null ixs   = error "not in list"
    | otherwise  = last ixs where
        ixs = indices x xs
    
    