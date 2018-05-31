import Prelude hiding ((.))
main::IO()
main = do

 print "Zadacha 1."
 print $ (^2) . (+1) $ 5
 print $ (+1) . (+1) $ 2
 print "Zadacha 2."
 print $ (fmin (**2) (2**)) 3
 print $ fmin (**2) (2**) $ 5
 print $ fmax (**2) (2**) $ 3
 print $ fmax (**2) (2**) 5  
 print $ favg (**2) (2**) 3  
 print $ favg (**2) (2**) 5 
 print "Zadacha 3."
 print $ boundUp (+1) 5 $ 5
 print $ boundUp (^3) 25 $ 2
 print "Zadacha 5."
 print $ maximize [(\x -> x ** 3), (\x -> x + 1)] 0.5
 print "Zadacha 6."
 print $ compose [(+1), (2*)] 7
 
{-
Задача 1. Дефинирайте инфиксния оператор (.) за композиция на функции. (Премахнат е от Prelude,
за да можем да си го дефинираме сами).

Пример:
    (^2) . (+1) $ 5   -> (5+1)^2 -> 36
    (+1) . (+1) $ 2   -> 4
    Извикваме оператора (.) върху две едноаргументни функции и получаваме като РЕЗУЛТАТ ТРЕТА ФУНКЦИЯ
    която е тяхната композиция. Върху този резултат от f . g, който е ФУНКЦИЯ, прилагаме
    някакъв аргумент x за да получим стойността на композицията в тази стойност на x.
-} 
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

{- 
Задача 2. Дефинирайте следните функции от по-висок ред:
а). fmin f g, която приема две едноместни числови функции f и gh
и връща едноместни числова функция, чиято стойност в точка x е
минимума на f и g.
б). fmax f g, като fmin, но връща максимума на f и g.
в). favg f g, като fmin, но връща средното аритметично на f и g.

f = (**2)
f = (\x -> x ** 2)
f x = x ** 2

g = (2**)
g = (\x -> 2 ** x)
g x = 2 ** x

Примери:
    Тук долара може да се изпусне но е важно да се запомни, че резултата
    на fmin f g е функция върху която прилагаме "3-тия" аргумент.
    (fmin f g) 3    -> 8
    fmin f g $ 5    -> 25
    fmax f g $ 3    -> 9
    fmax f g 5      -> 32
    favg f g 3      -> 8.5
    favg f g 5      -> (25 + 32) / 2 -> 28.5
-}

fmin :: (Double -> Double) -> (Double -> Double) -> (Double -> Double)
fmin f g = \x -> min (f x) (g x)

fmax :: (Double -> Double) -> (Double -> Double) -> (Double -> Double)
fmax f g = \x -> max (f x) (g x)

favg :: (Double -> Double) -> (Double -> Double) -> (Double -> Double)
favg f g = \x -> (f x + g x)/2

{-
Задача 3. Дефинирайте следните функции от по-висок ред:
а). boundUp f up, която приема едноместни числова функция f и
и числова стойност up и връща едноместни числова функция, чиято
стойност в точка x е минимума на f(x) и up.
б). boundDown f down, същата като boundUp, но връща максимума
на f(x) и down.

Примери:
    f = \x -> x + 1

    boundUp f 5 $ 1              -> 2
    boundUp f 5 $ 1              -> 2
    boundUp (\x -> x + 1) 5 $ 5  -> 5
    boundUp (^3) 25 $ 2          -> 8 
    boundUp (^3) 15 $ 3          -> 15 

    boundDown (^2) 5 $ 1         -> 5
    boundDown (^2) 5 $ 3         -> 9
-}
boundUp :: (Double -> Double) -> Double ->(Double -> Double)
boundUp f up = fmin f $ const up

boundDown :: (Double -> Double) -> Double -> (Double -> Double)
boundDown f down = fmax f $ const down

{-
Задача 5. Напишете функцията maximize, която получава непразен списък от едноместни
числови функции и връща нова едноместна числова функция на аргумент x, която дава 
стойността f x на тази фунция f от списъка, за която числото f x е най-голямо 
по абсолютна стойност.

Пример: 
    maximize [(\x -> x ** 3), (\x -> x + 1)] 0.5  = 1.5
    maximize [(\x -> x ** 3), (\x -> x + 1), (\x -> x ** 4)] (-2) = 16
-}
maximize :: [Double -> Double] -> (Double -> Double)
maximize [] _ = error "something is wrong bruh"
maximize [f] x = f x
maximize (f:fs) x = if abs fx > abs mfx then f x else mfx where
     fx = f x
     mfx = maximize fs x
     
{-
Задача 6. Напишете функцията compose fs, която приема списък от едноаргументни функцуии
и връща тяхната композиция, т.е. compose [f1, f2, .. fn] x = f1(f2( ... (fn(x))))

Пример: 
    compose [(+1), (2*)] 7 = (2 * 7) + 1 = 15
    compose [(+1), (+1), (+1)] 7 = 10
-}
compose :: [(Double -> Double)] -> (Double -> Double)
compose [] = id
compose (f:fs) = f.compose fs