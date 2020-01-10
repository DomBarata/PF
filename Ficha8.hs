module Ficha8 where

data Exp a = Const a
           | Simetrico (Exp a)
           | Mais  (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult  (Exp a) (Exp a)

e :: Exp Float
e = (Const 2.3) `Mult` ((Simetrico (Const 4.1)) `Mais` (Const 3))

instance Show a => Show (Exp a) where
  show = showExp

instance (Num a, Eq a) => Eq (Exp a) where
  (==) = igualExp

instance (Num a) => Num (Exp a) where
  (+) = somaExp
  (-) = subExp
  (*) = mulExp
  abs = absExp
  signum = sigNumExp
  fromInteger = deInt

showExp :: Show a => Exp a -> String
showExp (Const x)     = show x
showExp (Simetrico x) = "-" ++ showExp x
showExp (Mais a b)    = "(" ++ showExp a ++ " + " ++ showExp b ++ ")"
showExp (Menos a b)   = "(" ++ showExp a ++ " - " ++ showExp b ++ ")"
showExp (Mult a b)    = "(" ++ showExp a ++ " * " ++ showExp b ++ ")"

calcula :: Num a => Exp a -> a
calcula (Const x)     = x
calcula (Simetrico x) = - calcula x
calcula (Mais a b)    = (calcula a) + (calcula b)
calcula (Menos a b)   = (calcula a) - (calcula b)
calcula (Mult a b)    = (calcula a) * (calcula b)

igualExp :: (Num a, Eq a) => Exp a -> Exp a -> Bool
igualExp e1 e2 = calcula e1 == calcula e2

somaExp :: Num a => Exp a -> Exp a -> Exp a
somaExp e1 e2 = Const (calcula (Mais e1 e2))

subExp :: Num a => Exp a -> Exp a -> Exp a
subExp e1 e2 = Const (calcula (Menos e1 e2))

mulExp :: Num a => Exp a -> Exp a -> Exp a
mulExp e1 e2 = Const (calcula (Mult e1 e2))

absExp :: Num a => Exp a -> Exp a
absExp e = Const (abs (calcula e))

sigNumExp :: Num a => Exp a -> Exp a
sigNumExp e = Const (signum (calcula e))

deInt :: Num a => Integer -> Exp a
deInt i = Const (fromInteger i)
