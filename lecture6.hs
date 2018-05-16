import Data.Char
main::IO()
main=do

 print(listify [1,2,3,4])
 print(intersect [1,2,3] [1,3,7])
 print(subsets [1,2,3])
 
{-
Задача 2. Дефинирайте функцията listify xs, която разделя списъка xs
на спицъци, съдържащи всеки от индивидуалните елементи на xs.
-}
listify :: [a] -> [[a]]
listify xs = map(\x -> [x]) xs


{-
Задача 5. Сечение на множества: дефинирайте функцията intersect xs ys,
която приема два списъка, във всеки от които няма повтарящи се елементи
и връща сечението им.
-}
intersect :: Eq a => [a] -> [a] -> [a]
intersect xs ys = [x | x <- xs , x `elem` ys]

{-
Задача 6*. Дефинирайте функцията subsets xs, която връща списък с всички
подсписъци на списъка xs.

Пример:
    subsets [] = [[]]
    subsets [1, 2] = [[1, 2], [1], [2], []]
-}
subsets :: [a] -> [[a]]
subsets [] = [[]] 
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)