module Cores where

import Data.List

data Cores = Vermelho Int
           | Verde Int
           | Azul Int

v :: Cores
v = Vermelho 50

az :: Cores
az = Azul 82

cores = [v, az, Azul 2, Verde 15]

instance Show Cores where
  show = showCor

instance Eq Cores where
  (==) = igualCor

instance Ord Cores where
  (Vermelho i) <= (Vermelho j) = i <= j
  (Azul i) <= (Azul j)         = i <= j
  (Verde i) <= (Verde j)       = i <= j
  (Vermelho _) <= (Azul _)     = True
  (Verde _) <= (Vermelho _)    = True
  (Verde _) <= (Azul _)        = True
  _ <= _ = False



showCor :: Cores -> String
showCor (Azul i)     | i < 40             = "Azul suave"
                     | 40 <= i && i <= 80 = "Azul normal"
                     | i > 80             = "Azul Forte"
showCor (Vermelho i) | i < 40             = "Vermelho suave"
                     | 40 <= i && i <= 80 = "Vermelho normal"
                     | i > 80             = "Vermelho Forte"
showCor (Verde i)    | i < 40             = "Verde suave"
                     | 40 <= i && i <= 80 = "Verde normal"
                     | i > 80             = "Verde Forte"

igualCor :: Cores -> Cores -> Bool
igualCor (Azul _ ) (Azul _)         = True
igualCor (Verde _ ) (Verde _)       = True
igualCor (Vermelho _ ) (Vermelho _) = True
igualCor _ _                        = False
