import Test.QuickCheck
--ex1
produsRec :: [Integer] -> Integer
produsRec [] = 0
produsRec lista = product lista

produsFold :: [Integer] -> Integer
produsFold lista = foldr (*) 1 lista

prop_produs :: [Integer] -> Bool
prop_produs lista = (produsRec lista) == (produsFold lista) 

--ex2

andRec ::[Bool] -> Bool
andRec [] = True
andRec [True] = True
andRec [False] = False
andRec (x:xs) = if ( x == True) then andRec xs else False

andFold :: [Bool] -> Bool
andFold lista = foldr (&&) True lista

--ex3

concatRec :: [[a]] -> [a]
concatRec [[]] = []
concatRec lista = concat lista

concatFold :: [[a]] ->[a]
concatFold lista = foldr (++) [] lista 

--ex4 // stergere caracter din string
rmChar :: Char -> String -> String
rmChar ch lista =  [x | x <- lista, not(ch == x)]

rmCharRec :: String -> String -> String
rmCharRec lista [] = []
rmCharRec lista (h:t)
    | elem h lista = t'
    | otherwise  = h:t'
    where t' = rmCharRec lista t
    
rmCharFold :: String -> String -> String
rmCharFold lista1 lista2 = foldr rmChar lista2 lista1

---ex1Part2 foldr/l
semn :: [Integer] -> String
semn [] = []
semn (x:xs) = 
    if ( x >= -9 && x < 0) 
        then "-" ++ xs'
        else
            if( x >= 0 && x <= 9)
                then "+" ++ xs'
                else xs'
        where xs' = semn xs

semnFold :: [Integer] -> String
semnFold = foldr op unit
    where
        unit = []
        x `op` sir
            | x >= -9 && x < 0 = "-" ++ sir
            | x >= 0 && x <= 9 = "+" ++ sir
            | otherwise = sir
            
pozitiiPareFold ::[Integer] -> [Int]
pozitiiPareFold l = (foldr op unit l) 0
    where
        unit :: Int -> [Int]
        unit _ = []
        op :: Integer -> ( Int ->[Int] ) -> (Int -> [Int])
        (a `op` r) p 
            | even a = p : r (p+1)
            | otherwise = r (p+1)
        