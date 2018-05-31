import Data.List
main::IO()
main=do

let 
    {- 
    В примерите ще използваме следното дърво (tree): 

                                  3
                                /   \
                               1     4
                              / \   / \
                             0   2     5
                            / \ / \   / \
                                         6
    -}
    tree = (Node 3 (Node 1 (Node 0 Empty Empty) (Node 2 Empty Empty)) (Node 4 Empty (Node 5 Empty (Node 6 Empty Empty))))

{- Дефиниция на типа Tree a описващ произволно двоично дърво. -}
data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Read, Show)
    
{-
Пример 3. Да се дефинира функцията treeEmpty tree, която връща дали двоичното
дърво tree е празно.
-}
treeEmpty :: Tree a -> Bool
-- използваме съпоставяне по шаблон, с по един шаблон за всеки от случайте
treeEmpty Empty = True
treeEmpty _ = False

{-
Пример 4. Да се дефинира функцията treeRoot tree, която връща корена на
двоичното дърво tree.
-}
treeRoot :: Tree a -> a
treeRoot Empty = error "empty tree!"
treeRoot (Node val _ _) = val

{-
Пример 5. Да се дефинира функцията treeCount tree, която връща броя на
елементите на двоичното дърво tree.
-}
treeCount :: (Num b) => Tree a -> b
treeCount Empty = 0
treeCount (Node _ left right) = 1 + treeCount left + treeCount right


{-
Задача 3. Нека е дадено двоично дърво tree. Дефинирайте следните функции:

а). treeDepth tree, която връща дълбочината на дървото.
б). treeCountLeaves tree, която връща броя на листата на дървото.
в). treeSum tree, която връща сбора на всички стойности в дървото.
г). treeElem val tree, която проверява дали дадена стойност val е в дървото.
д). treeNodes tree, която връща списък със стойностите в дървото.
е). treeNodesAtLevel tree n, която връща списък със стойностите в n-тото ниво на дървото.
-}
treeDepth :: (Num b, Ord b) => Tree a -> b
treeDepth Empty = 0
treeDepth (Node _ left right) = 1 + max (treeDepth left) (treeDepth right)

treeCountLeaves :: (Num b) => Tree a -> b
treeCountLeaves Empty = 0
treeCountLeaves (Node _ Empty Empty) = 1
treeCountLeaves (Node _ left right) = treeCountLeaves left + treeCountLeaves right

treeSum :: Num a => Tree a -> a
treeSum Empty = 0
treeSum (Node x left right) = x + treeSum left + treeSum right

treeElem :: Eq a => a -> Tree a -> Bool
treeElem _ Empty = False
treeElem val (Node v left right) = v == val || treeElem val left || treeElem val right

treeNodes :: Tree a -> [a]
treeNodes Empty = []
treeNodes (Node x left right) = x : (treeNodes left ++ treeNodes right)

treeNodesAtLevel :: (Eq b, Num b) => Tree a -> b -> [a]
treeNodesAtLevel Empty _ = []
treeNodesAtLevel (Node x _ _) 0 = [x]
treeNodesAtLevel (Node _ left right) n = treeNodesAtLevel left (n - 1) ++ treeNodesAtLevel right (n - 1)
