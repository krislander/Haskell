main :: IO()
main = do
 print (getInterval 1 10)
 print (sumElems [1..10])
 print (countElems [1..10])
 print (memberOf 69 [1..10])
 print (removeFirst 6 [1..10])
 print (countOccurences 4 [1..10])
 
getInterval :: Int -> Int -> [Int]
getInterval a b = 
 if a==b then [a] else a:getInterval (a+1) b
 
sumElems :: [Int] -> Int
sumElems xs = 
    if null xs then 0 else head xs + sumElems (tail xs)
 
countElems :: [t] -> Int
countElems xs = 
  if null xs then 0 else 1 + countElems(tail xs)
  
memberOf :: Int -> [Int] -> Bool
memberOf _ [] = False
memberOf a xs
    |null xs      = False
    |a==(head xs) = True
    |otherwise    = memberOf a (tail xs)
    
removeFirst :: Int -> [Int] -> [Int]
removeFirst _ [] = []
removeFirst x (y:ys) = 
   if x==y then ys else y: removeFirst x ys
   
   
countOccurences :: Int -> [Int] -> Int
countOccurences _ [] = 0
countOccurences x (y:ys) = 
  (if x == y then 1 else 0) + countOccurences x ys