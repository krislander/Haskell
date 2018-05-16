main::IO()
main=do

 print (fact 5)
 print (mymax 12 13)
 print (mymin 2 3)
 print (myfunc 4 9)
 print (myfib 5)
 print (mymaxdivisor 15)
 print (isInside 15 25 100)
 print (isLeapYear 1984)
 print (numberOfDays 2 1995)
 print (isValidDate 31 1 1995)
 
 
 
--   This is factorial function
fact n =
   if (n==0) then 1 else (n*(fact(n-1)))

-- Minimum element
mymin x y = if(x<y) then x else y

-- Maximum Element
mymax x y = if(x>y) then x else y

-- Function to find average of two powered numbers
myfunc x y = (x^2+y^2) / 2

-- Find a fibonnaci number with rercursion
myfib :: Integer -> Integer
myfib n
 | n <= 1    = 1
 | otherwise = myfib(n-2)+ myfib(n-1)
 
-- Find if the number is inside the interval
isInside :: Integer->Integer->Integer->Bool
isInside x a b = (x >=a) && (x <= b)  

-- Find if the year is Leap
isLeapYear :: Integer -> Bool
isLeapYear year = 
    if year `mod` 4 == 0
       then if year `mod` 100 == 0
         then year `mod` 400 == 0
            else True
        else False
        
-- Find number of days in a month
numberOfDays :: Integer -> Integer -> Integer
numberOfDays month year
     | month == 1    = 31
     | month == 2    = if isLeapYear year then 29 else 28
     | month == 3    = 31
     | month == 4    = 30
     | month == 5    = 31
     | month == 6    = 30
     | month == 7    = 31
     | month == 8    = 31
     | month == 9    = 30
     | month == 10   = 31
     | month == 11   = 30
     | month == 12   = 31
     | otherwise     = error "1 <= month <= 12"
     
--Find if a date is valid
isValidDate :: Integer->Integer->Integer->Bool
isValidDate day month year = isValidMonth month && isInside day 1 (numberOfDays month year) where
    isValidMonth month = isInside month 1 12     
        
--Funkciq za namirane na nai-golqm delitel d na cqloto chislo x>1

mymaxdivisor:: Int->Int
mymaxdivisor x = helper2 (x-1) x

helper2 :: Int -> Int -> Int
helper2 d x = if mod x d == 0 then d else helper2 (d-1) x