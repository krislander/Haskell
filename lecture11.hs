import Data.List
main::IO()
main=do

 print(nodes [(1, 2), (1, 3), (2, 3), (2, 4)])
 print(neighbours [(1, 2), (1, 3), (2, 3), (2, 4)] 2)
 print(adjacencyList [(1, 2), (1, 3), (2, 3), (2, 4)])
 print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [1, 2, 4]
 print $ listLeaves [(1, 2, 3), (2, 4, 5)]
 print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 0 1
 
nodes :: (Eq a) => [(a, a)] -> [a]
nodes edges = nub $ map fst edges ++ map snd edges

neighbours :: (Eq a) => [(a, a)] -> a -> [a]
neighbours edges node = [to | (from, to) <- edges, from == node]

adjacencyList :: (Eq a) => [(a, a)] -> [(a, [a])]
adjacencyList edges = [(node, neighbours edges node) | node <- nodes edges]


findAssoc :: (Eq a) => [(a, b)] -> a -> b
findAssoc [] _ = error "not found!"
findAssoc ((k, v):xs) key
    | k == key  = v
    | otherwise = findAssoc xs key
    
isPath :: (Eq a) => [(a, [a])] -> [a] -> Bool
isPath adjs nodes = and [to `elem` findAssoc adjs from | (from, to) <- zip nodes (drop 1 nodes)]

{-
Задача 3. Нека е дадено двойчно дърво, представено като списък от тройки,
чиито първи елемент е идентификатор на дадения връх, а 2-рият и 3-тият,
идентификаторите на съответно лявото и дясно дете на върха.

Дефинирайте функцията listLeaves nodes, която връща списък с всички листа
на даденото дърво.

Примери:
    listLeaves [(1, 2, 3), (2, 4, 5)] -> [4, 3, 5]
    listLeaves [(2, 4, 5), (1, 2, 3)] -> [4, 5, 3]
-}
listLeaves :: (Eq a) => [(a,a,a)] ->[a]
listLeaves nodes = filterLeaves getLeftChild ++ filterLeaves getRightChild where
    getNodeId (nodeId, _, _) = nodeId
    getLeftChild (_, leftChild, _) = leftChild
    getRightChild (_, _, rightChild) = rightChild
    nodeIds = map getNodeId nodes
    filterLeaves getId = filter (\id -> not $ id `elem` nodeIds) $ map getId nodes

{-
Задача 4*. Дефинирайте функцията simplePaths adjs k node, която приема списък на
наследниците adjs на даден ориентиран граф, цяло число k и идентификатор на връх
node и връща всички прости пътища с дължина k, които започват от node.

Примери:
    simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 0 1 -> [[1]]
    simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 1 -> [[1, 2], [1, 3]]
    simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 2 1 -> [[1, 2, 3], [1, 2, 4]]
-}
simplePaths :: (Eq a) => [(a, [a])] -> Int -> a -> [[a]]
simplePaths adjs k node
    | k < 0     = error "k >= 0"
    | k == 0    = [[node]]
    | otherwise = map (node:) $ filter (not . (node `elem`)) $ concat [simplePaths adjs (k - 1) n | n <- findAssoc adjs node]

{-
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
-}