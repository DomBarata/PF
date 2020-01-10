module Ficha9 where

import System.Random
import Data.Char
import Data.String

bingo :: IO ()
bingo = do nl <- acumuladorNumero []
           print nl

acumuladorNumero :: [Int] -> IO [Int]
acumuladorNumero l | length l == 90 = return l
                   | otherwise =  do x <- randomRIO (1,90)
                                     getChar
                                     print x
                                     let nl = if elem x l
                                              then l
                                              else x:l
                                     acumuladorNumero nl

gerarChave :: IO (Int, Int, Int, Int)
gerarChave = do w <- randomRIO (0,9)
                x <- randomRIO (0,9)
                y <- randomRIO (0,9)
                z <- randomRIO (0,9)
                return ((w,x,y,z))

intruduzDigito :: IO (Int, Int, Int, Int)
intruduzDigito = do a:b:c:d:e <- getLine
                    return ((digitToInt a,digitToInt b,digitToInt c,digitToInt d))

quantosNumerosIguais :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Int
quantosNumerosIguais (a,b,c,d) (w,x,y,z) = length (filter (== True) [a==w, b==x, c==y, d==z])

joga :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> IO (Int, Int, Int, Int)
joga chave valores | quantosNumerosIguais chave valores == 4 = return chave
                   | otherwise = do print("O numero de numeros iguais e " ++ show (quantosNumerosIguais chave valores))
                                    novosValores <- intruduzDigito
                                    joga chave novosValores

mastermind :: IO ()
mastermind = do chave <- gerarChave
                palpite <- intruduzDigito
                ca <- joga chave palpite
                print("Acertou " ++ show ca)
