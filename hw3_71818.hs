import Data.List
main :: IO()
main = do

 print((pairCompose [(\x -> x+1),(\x -> x+2),(\x -> x+3)]) 1)
 print(switchSum (\x -> x + 1) (\x -> x * 2) 1 $ 2)
 print(switchSum (\x -> x + 1) (\x -> x * 2) 3 $ 2)
 print(replaceAssoc [5,4,2,3] [(1,5),(3,7),(5,9),(7,11),(9,13)])
 print(numOfNodes [(10,[3,7,12]),(3,[5,8,9]),(7,[11,13]),(12,[6,4]),(8,[1,2])])
 
{- 
Задача 1. Да се дефинира функция (pairCompose fs), която получава като
аргумент списък [f1,f2,f3, ... ,fn] с функции от тип Int -> Int и връща
нова едноаргументна числова функция g – такава, че оценката на (g x) е равна на
сумата (f1.f2) (x) + (f3.f4) (x) + ... + (fn-1.fn) (x), където “.” е
операторът за композиция на функции. Ако оригиналният списък с функции има
нечетен брой елементи, то последната функция от списъка се композира с функцията
идентитет (id).
Пример:
(pairCompose [(\x -> x+1),(\x -> x+2),(\x -> x+3)]) 
1 → ((1+2)+1)+(1+3) = 8
-}
pairCompose :: [(Int->Int)] -> (Int->Int)
pairCompose fss x
     | null fss = 0
     | length fss == 1 = (head fss) x
     | otherwise = (head fss.head(tail fss)) x + pairCompose (drop 2 fss) x


{-Задача 2. Ако f и g са числови функции и n е естествено число, да се дефинира
функция от по-висок ред switchsum f g n, която връща като резултат функция,
чиято стойност в дадена точка x е равна на f(x)+g(f(x))+f(g(f(x)))+...
(сумата включва n събираеми).
Примери:
switchsum (\x -> x + 1) (\x -> x * 2) 1 $ 2 → 3
switchsum (\x -> x + 1) (\x -> x * 2) 2 $ 2 → 9
switchsum (\x -> x + 1) (\x -> x * 2) 3 $ 2 → 16
switchsum (\x -> x + 1) (\x -> x * 2) 4 $ 2 → 30

-}
switchSum :: (Double->Double) -> (Double->Double) -> Int -> (Double->Double)
switchSum f g n = \x -> helper f g (f x) 0 0 where
   helper func1 func2 curr index res
    | index == n                       = res
    | index `mod` 2 == 0 || index == 0 = helper func1 func2 (g curr) (index+1) (curr+res)
    | otherwise                        = helper func1 func2 (f curr) (index+1) (curr+res)
    
{-Задача 3. Да се дефинира функция (replaceAssoc list dict), която получава
като аргументи списък list, чийто елементи са цели числа, и речник – асоциативен
списък dict, чийто елементи са двойки от цели числа. Функцията трябва да върне нов
списък, в който всеки елемент се получава чрез замяна на съответния елемент на list
с асоциираната с него стойност в dict, ако в dict съществува елемент с такъв ключ,
или е равен на съответния елемент на list – в противен случай.
Пример:
(replaceAssoc [5,4,2,3] [(1,5),(3,7),(5,9),(7,11),(9,13)]) →
 [9,4,2,7]
 -}
replaceEachTuple :: Integer -> [(Integer, Integer)] -> Integer
replaceEachTuple num dict
    | null dict             = num
    | num == fst(head dict) = snd (head dict)
    | otherwise             = replaceEachTuple num (tail dict)

replaceAssoc :: [Integer] -> [(Integer, Integer)] -> [Integer]
replaceAssoc list dict = helper list dict [] where
   helper ls dc res
     | null ls = res
     | otherwise = helper (tail ls) dc  (res ++ [(replaceEachTuple (head ls) dc)])
     --NE MOGA DA RAZBERA ZASHTO GO PECHATA NAOBRATNO......
    
{-Задача 4. Дадено е дърво tree от цели числа, представено с асоциативен списък,
описващ преките наследници (синовете) на върховете, които не са листа. Да се
дефинира функция (numOfNodes tree), която намира броя на вътрешните върхове
node на tree, за които сумата на синовете на node е равна по стойност на родителя
на node.
Пример:
(numOfNodes [(10,[3,7,12]),(3,[5,8,9]),(7,[11,13]),
 (12,[6,4]),(8,[1,2])]) → 2 (върховете 12 и 8)  
-}
numOfNodes :: [(Integer,[Integer])] -> Integer
numOfNodes tree = helper tree (snd(head tree)) 0 where
   helper tr curr res
      | null tr                                                             = res 
      | null curr                                                           = helper (tail tr) (snd(head (tail tr))) res
      | sum(concat[b | (a ,b) <- tr, a == head(curr)]) == fst (head tr) = helper tr (tail curr) (res + 1)
      | otherwise = helper tr (tail curr) res