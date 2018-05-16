import Data.List
main::IO()
main=do

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


