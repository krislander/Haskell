import Prelude hiding (foldl, foldr, sum, product, length, any, all, minimum, maximum, concat, reverse, filter)
import Data.List
main::IO()
main=do


--VAJNO LQVO I DQSNO RAZKLONENIE FOLDLeft i FoldRight
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc []     = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc []     = acc
foldr f acc (x:xs) = f x (foldr f acc xs)

{-
Примери:
    sum [1..10] = 55
    product [1..10] = 3628800
    length [1..10] = 10
    any even [1..10] = True
    all even [1..10] = False
    minimum [1..10] = 1
    maximum [1..10] = 10
-}
sum :: Num a => [a] -> a
sum xs = foldr (+) 0 xs

product :: Num a => [a] -> a
product xs = foldr (*) 1 xs

length :: Num b => [a] -> b
length xs = foldr (\_ acc -> acc + 1) 0 xs

any :: (a -> Bool) -> [a] -> Bool
any f xs = foldr (||) False (map f xs)

all :: (a -> Bool) -> [a] -> Bool
all f xs = foldr (&&) True (map f xs)

minimum :: Ord a => [a] -> a
minimum [] = error "empty list"
minimum (x:xs) = foldr min x xs

maximum :: Ord a => [a] -> a
maximum [] = error "empty list"
maximum (x:xs) = foldr max x xs
{-
Задача 2. foldl/foldr могат да връщат и списъци: използвайте foldl/foldr, 
за да дефинирате следните функции:
а). concat xss, която приема списък от списъци xss и ги конкатенира в един общ списък.
б). reverse xs, която приема списък xs и обръща елементите му.

Примери: 
    concat [[1], [2], [3]] = [1, 2, 3]
    reverse [1, 2, 3] = [3, 2, 1]
-}
concat :: [[a]] -> [a]
concat xs = foldr (++) [] xs

reverse :: [a] -> [a]
reverse xs = foldl (flip (:)) [] xs
reverse' xs = foldl (\acc x -> x:acc) [] xs

{-
Задача 3. Напишете функцията compose fs, която приема списък от едноаргументни функцуии
и връща тяхната композиция, т.е. compose [f1, f2, .. fn] x = f1(f2( ... (fn(x))))

Този път използвайте fold.

Пример: 
    compose [(+1), (2*)] 7 = (2 * 7) + 1 = 15
-}
compose :: [(a -> a)] -> (a -> a)
compose fs = foldl (.) id fs








 print(sumMatrix [[1,2,3],[2,3,4],[3,4,5]] [[5,4,3],[4,3,2],[3,2,1]] )
 print(nullMatrix [[1,0,3],[2,3,4],[0,4,5]] )
 print(transposeMatrix [[1,0,3],[2,3,4],[0,4,5]] )
 print(productMatrix [[1,0,3],[2,3,4],[0,4,5]] [[3,1,2],[2,1,3],[2,2,2]] )
 print(grafMatrix [[1,0,3],[2,3,4],[0,4,5]])
 print(getLeafs 
{-
  Зад. 1. Да се напише функция, която намира сбора на две матрици, представени
  като списък от списъци.
-} 
sumMatrix :: Num a => [[a]] -> [[a]] -> [[a]]
sumMatrix = zipWith (zipWith (+))

{-
  Зад. 2. Да се напише функция, която нулира всички сълбове на матрица, в които
  се съдържа стойност 0. Матрицата е представена като списък от списъци.
-}
nullColumn :: (Num a, Eq a) => [a] -> [a]
nullColumn xs = if elem 0 xs then map (*0) xs else xs

nullMatrix :: (Num a, Eq a) =>[[a]] -> [[a]]
nullMatrix xss@([]:_) = xss
nullMatrix xss =
     zipWith (\ x xs -> x:xs) (nullColumn (map head xss)) (nullMatrix (map tail xss))

{-
  Зад. 3. Напишете функция която намира транспонираната на дадена матрица.
-}
transposeMatrix :: Num a => [[a]] -> [[a]]
transposeMatrix [] = []
transposeMatrix ([]:_) = []
transposeMatrix x = (map head x) : transposeMatrix (map tail x)

{-
  Зад. 4. Напишете функция, която намира произведението на две матрици.
-}
productMatrix :: Num a => [[a]] -> [[a]] -> [[a]]
productMatrix a b = [[sum (zipWith (*) x y )| y <- (transposeMatrix b)] | x <- a]

{-
  Зад. 5. Напишете функция, която намира броя на дъгите на граф, представен
  чрез матрица на съседство.
-}
grafMatrix :: [[Int]] -> Int
grafMatrix xss = sum (map sum xss)

grafMatrix2 :: [[Int]] -> Int
grafMatrix2 = sum . (map sum)

{-
  Зад. 6. Напишете функция, която намира всички листа на дърво, представено
  чрез матрица на съседство.
-}
getLeafs :: [[Int]] -> [Int]
getLeafs 


