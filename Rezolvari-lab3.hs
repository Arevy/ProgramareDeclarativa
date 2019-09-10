import Test.QuickCheck
--1
factori :: Int -> [Int]
factori n = [x | x<- [2..n`div`2], n`mod`x == 0]
--2
prim :: Int -> Bool
prim n = if length(factori n) == 0 then True else False
--3
numerePrime :: Int -> [Int]
numerePrime n = [ x | x<-[2..n] , (prim x) == True]
--L3.2

myzip3 :: [Integer] -> [Integer] -> [Integer] -> [(Integer, Integer, Integer)]
myzip3 _ [] _ = []
myzip3 [] _ _ = []
myzip3 _ _ [] = []
myzip3 (h1:t1) (h2:t2) (h3:t3) = (h1,h2,h3): (myzip3 t1 t2 t3) 

--- 1
ordonataNat :: [Integer] -> Bool
ordonataNat [] = True
ordonataNat [x] = True
ordonataNat (x:xs) = and [ x <= head xs && ordonataNat xs]
---2
ordonataNat1 :: [Integer] -> Bool
ordonataNat1 [] = True
ordonataNat1 [x] = True
ordonataNat1 (x:xs) = if (x <= head xs) then ordonataNat1 xs else False

testOrdonata x = (ordonataNat x) == (ordonataNat1 x) 

--L3.4
{-
ordonata :: [a] -> (a -> a -> Bool) -> Bool
ordonata [] operatie = True
ordonata [] _ = False
ordonata (x:xs) operatie = if(x operatie head xs) then ordonata xs operatie else False 
-}
---3
(*<*) :: (Integer, Integer) -> (Integer, Integer) -> Bool
a *<* b = fst a == fst b && snd a == snd b

--Map--1
firstEl :: [(a,b)] -> [a]
firstEl xs = map (fst) xs
--2
sumList :: [[Integer]] -> [Integer]
sumList xs = map (sum) xs
--3
prel2 :: [Integer] -> [Integer]
prel2 [] = [] 
prel2 (x:xs) 
    | x`mod`2 == 0 = (x`div`2) : xs'
    | otherwise = (x*2) : xs'
    where xs' = prel2 xs

--4
compuneList :: (b -> c) -> [(a -> b)] -> [(a -> c)]
compuneList f lista = [f.e | e <- lista]

aplicaList :: a -> [(a -> b)] -> [b]
aplicaList e lista = [func e | func <- lista]

--L3.6
--1
caracter :: Char -> [String] -> [String]
caracter _ [] = []
caracter ch xs = filter(elem ch) xs
--2
patrate_impare :: [Integer] -> [Integer]
patrate_impare [] = []
patrate_impare (x:xs) =   
    let 
        lista = filter(odd) (x:xs)
    in
        map (^2) lista
--3
functie :: [Integer] -> [Integer]
functie list = [ fst x * fst x | x <- filter (\x -> odd(snd x)) (zip list [1..]) ]

    
    



