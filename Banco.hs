module Banco where

data Movimento = Credito Float
               | Debito Float

data Data = D Int Int Int

data Extracto = Ext Float [(Data, String, Movimento)]

instance Show Movimento where
  show = showMov

instance Show Data where
  show = showDat

instance Show Extracto where
  show = showExtrato

e = Ext 300 [((D 2010 4 5), "DEPOSITO", Credito 2000),(D 2010 8 10, "COMPRA", Debito 37.5), (D 2010 9 1, "LEV", Debito 60),(D 2011 1 7, "JUROS", Credito 100), (D 2011 1 22, "ANUIDADE", Debito 8)]


filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext i ((d,s,m):t)) l | elem s l  = (d,m): filtro (Ext i t) l
                             | otherwise = filtro (Ext i t) l
filtro _ _ = []

debitos :: Extracto -> [(Data, String, Movimento)]
debitos (Ext _ mvs) = filter (\(_, _, m) -> eDebito m) mvs

eDebito :: Movimento -> Bool
eDebito (Credito _) = False
eDebito (Debito _)  = True

datas :: Extracto -> [Data]
datas (Ext _ mvs) = map (\(d,_,_) -> d) mvs

showMov :: Movimento -> String
showMov (Credito x) = "+" ++ show x
showMov (Debito x)  = "-" ++ show x

showDat :: Data -> String
showDat (D aaaa mm dd) = show (aaaa) ++ "/" ++ show (mm) ++ "/" ++ show (dd)

showListas :: [(Data, String, Movimento)] -> String
showListas [] = "\n"
showListas ((d, s, m):t) |eDebito m = "\n" ++ showDat d ++ "   " ++ s ++ "         " ++ showMov m ++ showListas t
                         |otherwise = "\n" ++ showDat d ++ "   " ++ s ++ showMov m ++ showListas t

calculaSaldo :: Float -> [(Data, String, Movimento)] -> Float
calculaSaldo v (((_,_,(Credito x)):t)) = calculaSaldo (v+x) t
calculaSaldo v (((_,_,(Debito x)):t)) = calculaSaldo (v-x) t
calculaSaldo v _ = v

showExtrato :: Extracto -> String
showExtrato (Ext i l) = "Saldo anterior: " ++ show i ++ "EUR\n" ++
                         "---------------------------------------\n" ++
                         "Data       Descricao   Credito   Debito\n" ++
                         "---------------------------------------" ++
                         showListas l ++
                         "---------------------------------------\n" ++
                         "Saldo actual: " ++ show (calculaSaldo i l)
