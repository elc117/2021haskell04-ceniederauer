-- Prática 04 de Haskell
-- Nome: Carlos Eduardo Niederauer Rodrigues

import Text.Printf

faixaIdoso :: Int -> String
faixaIdoso idade 
  |idade >= 60 && idade <= 64 = "IDO64"
  |idade >= 65 && idade <= 69 = "IDO69"
  |idade >= 70 && idade <= 74 = "IDO74"
  |idade >= 75 && idade <= 79 = "IDO79"
  |idade >= 80 = "IDO80"
  |otherwise = "[ERROR] Idade incorreta"

classifIdosos :: [(String,Int)] -> [(String,Int,String)]
classifIdosos lis = zip[fst x | x <- lis] [snd y | y <- lis] [faixaIdoso (snd y) | y <- lis]

classifIdosos' :: [(String,Int)] -> [(String,Int,String)]
classifIdosos' lis = zip (map fst lis) (map snd lis) (map faixaIdoso(map snd lis))

strColor :: (Int,Int,Int) -> String
strColor (r,g,b) = printf "rgb(%d,%d,%d)" r g b

genCircs :: Int -> (Int,Int) -> Int -> [(Int,Int,Int)]
genCircs n (cx,cy) r = take n [(x,cy,r) | x <- (iterate (+2) cx)]

genReds :: Int -> [(Int,Int,Int)]
genReds i = take n [(x,0,0) | x <- [2,2 + truncate (fromIntegral i*1.33)..], x < 255]