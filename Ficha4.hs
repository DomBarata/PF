module Ficha4 where

  import Data.Char

  digitAlpha :: String -> (String,String)
  digitAlpha []    = ([], [])
  digitAlpha (h:t) | isDigit h = (h:a, b)
                   | isAlpha h = (a, h:b)
                   | otherwise = (a, b)
       where (a,b) = digitAlpha t

  nzp :: [Int] -> (Int,Int,Int)
  nzp []          = (0, 0, 0)
  nzp (h:t)       | h < 0     = (n+1, z, p)
                  | h == 0    = (n, z+1, p)
                  | otherwise = (n, z, p+1)
                  where (n, z, p) = nzp t

  nzp' :: [Int] -> (Int,Int,Int) -> (Int,Int,Int)
  nzp' [] r            = r
  nzp' (h:t) (n, z, p) | h < 0     = nzp' t (n+1, z, p)
                       | h == 0    = nzp' t (n, z+1, p)
                       | otherwise = nzp' t (n, z, p+1)

  divMode :: Integral a => a -> a -> (a, a)
  divMode x y | x < y     = (0, x)
              | otherwise = let (q, r) = divMode(x-y) y
                            in  (q+1, r)

  fromDigits :: [Int] -> Int
  fromDigits []    = 0
  fromDigits (h:t) = h*10^(length t) + fromDigits t

  fromDigits' :: [Int] -> Int
  fromDigits' l = fst (aux l)

  aux :: [Int] -> (Int, Int)
  aux []       = (0, 0)
  aux (h:t)    = (h*10^i+v, i+1)
   where (v,i) = aux t







  l :: [Int]
  l = [1, 2, 3, 4]

  inits' :: [a] -> [[a]]
  inits' [x] = [[], [x]]
  inits' l = x++[l]
     where x = inits' (take ((length l)-1) l)
