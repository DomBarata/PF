module RoseTree where

data RTree a = R a [RTree a]
                deriving Show

rt :: RTree Int
rt = R 7 [R 8 [R 1 [R 5 []], R 2 [], R 4 [R 23 [], R 77 []]], R 5 []]

soma :: Num a => RTree a -> a
--soma (R x []) = x
soma (R x l) = x + sum (map soma l)

altura :: RTree a -> Int
altura (R x l) = 1 + foldr max 0 (map altura l)

prune :: Int -> RTree a -> RTree a
prune 1 (R x l) = R x []
prune n (R x l) = R x (map (prune (n-1)) l)

mirror :: RTree a -> RTree a
mirror (R x l) = R x (reverse (map mirror l))

postorder :: RTree a -> [a]
postorder (R x []) = [x]
postorder (R x l)  = concatMap postorder l ++ [x]

postorder' :: RTree a -> [a]
postorder' (R i []) = [i]
postorder' (R i ts) = foldr (\rt r -> postorder' rt ++ r) [i] ts
