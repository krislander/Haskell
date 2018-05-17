import Data.List
main::IO()
main=do

 print(nodes [(1, 2), (1, 3), (2, 3), (2, 4)])
 print(neighbours [(1, 2), (1, 3), (2, 3), (2, 4)] 2)
 print(adjacencyList [(1, 2), (1, 3), (2, 3), (2, 4)])
 
nodes :: (Eq a) => [(a, a)] -> [a]
nodes xs = [(snd x) | x <- xs] 

neighbours :: (Eq a) => [(a, a)] -> a -> [a]
neighbours xs n = [b| (a,b) <-xs , a == n]

adjacencyList :: (Eq a) => [(a, a)] -> [(a, [a])]
adjacencyList xs = [(a, [b|(a,b) <- xs])

--Uprajnenie 12

data List a = Nil | a `Cons` (List a)
     deriving Show
mkList :: [a] -> List a
mkList []     = Nil
mkList (x:xs) = x `Cons` (unList xs)

unList :: List a -> [a]
unList Nil       = []
unList (Cons x xs) = x : (unList xs)

listEmpty::List a -> Bool
listEmpty Nil = True
listEmpty_ = False

listHead :: List a -> a
listHead (Cons x _) = x

listTail :: List a -> List a
listTail (Cons _ xs) = xs

listMap :: (a -> b) List a ->List b
listMap _ Nil              = Nil
listMap f (x `Cons` xs) = (Cons (f x) (listMap f xs))

listFilter :: (a -> Bool) -> List a -> List a
listFilter p (x `Cons` xs) = if p x then Cons x (listFilter p  xs) else listFilter p xs