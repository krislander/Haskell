main::IO()
main=do

 print("123")
 print (fact 5)
 print (mymax 12 13)
 print (mymin 2 3)
 print (myfunc 4 9)
 print (myfib 5)
 print (mymaxdivisor 15)
 
 
--   This is factorial function
fact n=if (n==0) then 1 else (n*(fact(n-1)))

--  Minimum element
mymin x y = if(x<y) then x else y

--   Maximum Element
mymax x y = if(x>y) then x else y

-- Function to find average of two powered numbers
myfunc x y = (x^2+y^2) / 2

--Find a fibonnaci number with rercursion
myfib :: Integer -> Integer
myfib n
 | n<=1 =1
 | otherwise = myfib(n-1)+ myfib(n-2)


--Funkciq za namirane na nai-golqm delitel d na cqloto chislo x>1

mymaxdivisor:: Int->Int
mymaxdivisor x = helper2 (x-1) x

helper2 :: Int -> Int -> Int
helper2 d x = if mod x d == 0 then d else helper2 (d-1) x

