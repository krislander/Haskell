main::IO()
main = do


 print (isSpecial 91 99)
 print (maxSquare [sqrt, (\x -> x + 2), (\x -> 2 * x)] 5)
 print (closestAverage [(Temp 1 23.6),(Temp 6 24.2), 
                  (Temp 11 24.2),(Temp 16 21.2),
                  (Temp 21 23.8),(Temp 26 26.5),
                  (Temp 31 24.5)])
 
--Zadacha 1
isPrime::Int->Bool
isPrime n = n > 1 && null [i | i <-[2 .. n-1], n `mod` i == 0]

reverseDigits :: Int -> Int
reverseDigits n = helper n 0 where
     helper n res
         |n == 0    = res
         |otherwise = helper (n `div` 10)(10*res + n `mod` 10)
         
isSpecial :: Int -> Int -> [Int]
isSpecial h k = helper h k [] where
    helper start end lst
       |start>end  = lst
       |isPrime (reverseDigits start) == True = helper (start+1) end (start:lst)
       |otherwise = helper (start + 1) end lst
       
       
--Zadacha 2
argmax :: (Ord b) => (a -> b) -> [a] -> a
argmax f xs = head [x | x <- xs, f x == fmax] where
      fmax = maximum $ map f xs
      
maxSquare :: (Ord a, Num a) => [(a -> a)] -> (a -> a)
maxSquare fs = \x -> (helper fs x) x where
      helper []_ = error "no sauce!"
      helper [f]_ = f
      helper (f:fs) x = if f (x^2) > fmax (x^2) then f else fmax where
           fmax = helper fs x
           
           
--Zadacha 3
data Measuring = Temp Int Float
    deriving (Read, Show)

argmin :: (Ord b) => (a -> b) -> [a] -> a
argmin f xs = head [x |x <- xs, f x == fmin] where
    fmin = minimum $ map f xs
    
getDay :: Measuring -> Int
getDay (Temp day _) = day

getTemp :: Measuring -> Float
getTemp (Temp _ temp) = temp

closestAverage:: [Measuring] -> Int
closestAverage measurings = getDay(argmin (\m -> abs (getTemp m - avgTemp)) measurings) where
    avgTemp = sum (map getTemp measurings) / fromIntegral (length measurings)