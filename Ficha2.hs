module Ficha2 where

  dobros :: [Float] -> [Float]
  dobros []    = []
  dobros (h:t) = 2*h : (dobros t)

  numOcorre :: Char -> String -> Int
  numOcorre c []    = 0
  numOcorre c (h:t) = if c == h then 1 + numOcorre c t
                      else numOcorre c t

  positivos :: [Int] -> Bool
  positivos [l]   = l > 0
  positivos (h:t) = if h > 0 then positivos t
                    else False

  soPos :: [Int] -> [Int]
  soPos []    = []
  soPos (h:t) = if h >= 0 then h:(soPos t)
                else soPos t

  somaNeg :: [Int] -> Int
  somaNeg []    = 0
  somaNeg (h:t) = if h < 0 then h + (somaNeg t)
                  else somaNeg t

  tresUlt :: [a] -> [a]
  tresUlt (h:t) = if length (h:t) <= 3 then (h:t)
                  else tresUlt t

  tresUlt' :: [a] -> [a]
  tresUlt' (h:t:x:[]) = [h,t,x]
  tresUlt' (h:t)      = tresUlt' t
  tresUlt' l          = l

  segundos :: [(a,b)] -> [b]
  segundos []          = []
  segundos ((h1,h2):t) = h2:(segundos t)

  nosPrimeiros :: Eq a => a -> [(a,b)] -> Bool
  nosPrimeiros n []    = False
  nosPrimeiros n (h:t) | n == fst h = True
                       | otherwise  = nosPrimeiros n t

  sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
  sumTriplos []             = (0,0,0)
  sumTriplos ((h1,h2,h3):t) = (h1+x,h2+y,h3+z)
              where (x,y,z) = sumTriplos (t)
