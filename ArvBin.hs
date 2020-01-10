module ArvBin where

  data BTree a = Empty
               | Node a (BTree a) (BTree a)
               deriving Show

  t = Node 7 (Node 3 Empty (Node 8 Empty Empty)) (Node 2 Empty Empty)
  w = Node (7, "atum", "pao") (Node (3, "camarao", "marisco") Empty (Node (8, "pepsi", "fanta") Empty Empty)) (Node (2, "salsicha", "requeijao") Empty Empty)
  u = Node 15 (Node 20 Empty Empty) (Node 7 Empty Empty)

  altura :: BTree a -> Int
  altura Empty        = 0
  altura (Node r e d) = 1 + max (altura e) (altura d)

  contaNodos :: BTree a -> Int
  contaNodos Empty        = 0
  contaNodos (Node r e d) = 1 + contaNodos e + contaNodos d

  folhas :: BTree a -> Int
  folhas Empty                = 0
  folhas (Node r Empty Empty) = 1
  folhas (Node r e d)         = folhas e + folhas d

  prune :: Int -> BTree a -> BTree a
  prune 0 _            = Empty
  prune _ Empty        = Empty
  prune n (Node r e d) = Node r (prune (n-1) e) (prune (n-1) d)

  path :: [Bool] -> BTree a -> [a]
  path [] (Node r e d)        = [r]
  path _ Empty                = []
  path (True:t) (Node r _ d)  = r:path t d
  path (False:t) (Node r e _) = r:path t e

  mirror :: BTree a -> BTree a
  mirror Empty = Empty
  mirror (Node r e d) = Node r (mirror d) (mirror e)

  zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
  zipWithBT f (Node r e d) (Node r1 e1 d1) = Node (f r r1) (zipWithBT f e e1) (zipWithBT f d d1)
  zipWithBT _ _ _                          = Empty

  unzipBT :: BTree (a,b,c) -> (BTree a, BTree b, BTree c)
  unzipBT Empty = (Empty, Empty, Empty)
  unzipBT (Node (a,b,c) e d) = (Node a e1 d1, Node b e2 d2, Node c e3 d3)
          where (e1, e2, e3) = unzipBT e
                (d1, d2, d3) = unzipBT d
