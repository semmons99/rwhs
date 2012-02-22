-- file ch03/Exercises.hs

import Data.List
import Data.Function

len :: [a] -> Int
len (x:xs) = 1 + (len xs)
len [] = 0

mean xs = (sum xs) / (fromIntegral (length xs))

palindrome (x:xs) = [x] ++ (palindrome xs) ++ [x]
palindrome [] = []

isPalindrome [] = True
isPalindrome xs = head == reverse tail
  where size  = length xs
        pivot = div size 2
        head  = (take pivot xs)
        tail  = if (odd size) then
                  (drop (pivot + 1) xs)
                else
                  (drop pivot xs)

sortBySize xs = sortBy (compare `on` length) xs

join _   []     = []
join _   (x:[]) = x
join sep (x:xs) = x ++ sep ++ (join sep xs)

data BinaryTree a = Node a (BinaryTree a) (BinaryTree a)
                  | Empty
                    deriving (Show)

tree1 = Empty
tree2 = Node "x" Empty Empty
tree3 = Node "x" Empty (Node "y" Empty Empty)
tree4 = Node "x" Empty (Node "y" Empty (Node "z" Empty Empty))

btreeHeight Empty = 0
btreeHeight (Node _ a b) = 1 + tallestChild
  where tallestChild = max (btreeHeight a) (btreeHeight b)
