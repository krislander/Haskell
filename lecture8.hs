import Data.List
main::IO()
main = do

 print (zeta 1000 2)
 print (isImageOf (\x -> x ^ 2) [1, 2, 3, 4] [1, 4, 9, 16])
 print (argmin snd [("Penka", 23), ("Gosho", 19), ("Kiro", 21)])
 print (coldestMonth [
            (1, 1, 0), (1, 10, -5), (1, 20, 8), 
            (2, 1, 0), (2, 10, -5), (2, 20, 0),
            (3, 1, 5), (3, 10, 10), (3, 20, 8),
            (4, 1, 9), (4, 10, 15), (4, 20, 18)
        ])
 print (getDecreasing [[5, 1, 2, 3, 4], [1, 1, -1, -2, -3], [4, 3, 2, 1], [7, 6, 5]])       
 
{-
Задача 1. Дефинирайте функцията zeta n x, която приема целочисления аргумент n и реалното число s
и връща сбора на първите n члена на редицата 1, 1 / 2 ^ x, .. 1 / k ^ x ..
-}
zeta :: Int -> Double -> Double
zeta n x 
    | n <= 0    = 0
    | otherwise = 1 / fromIntegral n ** x + zeta (n-1) x

{-
Задача 2. Нека са дадени множествата xs и ys, и едноаргументата функция f от xs към ys.
Ще наричаме множеството ys образ на xs през f, тогава и само тогава когато за всяко x от xs,
f(x) принадлежи на ys и за всяко y от ys, съществува x, елемент на xs, за който f(x) = y.

Дефинирайте функцията isImageOf f xs ys, която връща дали ys е образ на xs през f.
-}
isImageOf :: Eq b => (a -> b) -> [a] -> [b] -> Bool
isImageOf f xs ys = and [fx `elem` ys | fx <- fxs] && and [y `elem` fxs | y <- ys] where
   fxs = map f xs
   
{-
Задача 3. Напишете функцията argmin f xs, която връща елемента x от списъка xs, която
минимизира функцията f, т.е. това x при което f x има най-малка стойност.

Пример:
    Ако имаме people = [("Пенка", 23), ("Гошо", 19), ("Киро", 21)] и искаме да върнем наредената двойка
    от имена и възрасти с най-малка стойност, то можем да извикаме:
    
    argmin snd people                   -> ("Гошо", 19)
    argmin (\(_, age) -> age) people    -> ("Гошо", 19)
-}
argmin :: Ord b => (a -> b) -> [a] -> a
argmin f xs = head (filter (\x -> f x == minimizingArg) xs) where
   minimizingArg = minimum (map f xs)
  

--this is the legit one bruh
argminn :: Ord b => (a -> b) -> [a] -> a
argminn f xs = head [x | (x, fx) <- zip xs fxs, fx == m] where
    fxs = map f xs
    m = minimum fxs
    
{-
Задача 4. Нека са дефинирани следните типове:

type Temperature = Float
type Day = Int
type Month = Int
type Record = (Month, Day, Temperature)

Дефинирайте функцията coldestMonth records, която приема списък от температурни измервания
и връща, кой е бил най-студения месец (т.е. месецът с най-ниска средна температура)

Съвет: Подобен тип задачи най-лесно се решават като се разбият на подзадачи.

Пример:
    coldestMonth [
            (1, 1, 0), (1, 10, -5), (1, 20, 8),
            (2, 1, 0), (2, 10, -5), (2, 20, 0),
            (3, 1, 5), (3, 10, 10), (3, 20, 8),
            (4, 1, 9), (4, 10, 15), (4, 20, 18)
        ]
            -> 2
-}
type Temperature = Float
type Day = Int
type Month = Int
type Record = (Month,Day,Temperature)

--variant 1
averageMonthlyTemp :: [Record] -> Month -> Temperature
averageMonthlyTemp records month = sum ts / (fromIntegral (length ts)) where
   ts = [t | (m,d,t) <- records, m == month]
   
coldestMonth :: [Record] -> Month
coldestMonth records = argmin(\month -> averageMonthlyTemp records month) [m | (m, _, _) <- records]

{-
Задача 6. Дефинирайте функция getDecreasing, която за даден списък xss, елементите на който са
непразни списъци от числа, връща като резултат списък от тези елементи на xss, които представляват
строго намаляваща редица.

Пример:
    getDecreasing [[5, 1, 2, 3, 4], [1, 1, -1, -2, -3], [4, 3, 2, 1], [7, 6, 5]]
                    -> [[4, 3, 2, 1], [7, 6, 5]]
-}
--parvo pishem function dali e strogo namalqvashta
isStrictlyDecreasing :: Ord a => [a] -> Bool
isStrictlyDecreasing xs = and [x1 > x2 | (x1, x2) <- zip xs (drop 1 xs)]

getDecreasing :: Ord a => [[a]] -> [[a]]
getDecreasing xss = filter isStrictlyDecreasing xss