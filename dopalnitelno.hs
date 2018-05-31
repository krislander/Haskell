import Data.List 
main::IO()
main=
  let 
    tree = (Node 3 (Node 1 (Node 0 Empty Empty) (Node 2 Empty Empty)) (Node 4 Empty (Node 5 Empty (Node 6 Empty Empty))))
     in do
 


{-
Дефинирайте функцията heavyNodes tree, която връща списък със стойностите на всички върхове
на дървото tree, които са по-големи от сбора на предшествениците си.
-}
heavyNodes :: Tree -> [Int]
heavyNodes tree = helper tree 0 where
    helper Empty _ = []
    helper (Node v left right) psum = (if v > psum then (v:) else id) heavyChildren where
        heavyChildren = helper left (v + psum) ++ helper right (v + psum)