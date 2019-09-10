import Test.QuickCheck
--Fibonacci
fibonacciEcuational :: Integer -> Integer
fibonacciEcuational 0 = 0
fibonacciEcuational 1 = 1
fibonacciEcuational n = fibonacciEcuational (n-1) + fibonacciEcuational (n-2)

--L2.1
fibonacciPereche :: Integer  -> (Integer,Integer)
fibonacciPereche 1 = (0,1)
fibonacciPereche n = (n-2,n-1)

fibonacciLiniar :: Integer -> Integer
fibonacciLiniar 0 = 0
fibonacciLiniar n = snd (fibonacciPereche(n))


{-prop_fibonacci :: Integer -> Property
prop_fibonacci n = n >= 0 ==> fibonacciEcuational n == fibonacciLiniar n-}

--L2.3

{-
inInterval :: Integer -> Integer -> [Integer] -> [Integer]
inInterval _ _ [] = []
inInterval x y [] = []
inInterval x y (h:t) 
    | h >= x && h <= y = h:t'
    | otherwise = t'
    where t' = inInterval x y t
-}

--L2.4

inIntervalComp :: Integer -> Integer -> [Integer] -> [Integer]
inIntervalComp x y xs = [ a | a <- xs , a >= x && a <= y ]

pozitiveRec :: [Integer] -> Integer
pozitiveRec [] = 0
pozitiveRec (h:t) 
    | h > 0 = 1+t'
    | otherwise = t'
    where t' = pozitiveRec t
    
pozitiveRecComp ::[Integer] -> Integer
pozitiveRecComp xs = sum [1 | x <- xs, x > 0]

testPozitive x = (pozitiveRec x) == (pozitiveRecComp x) 

--L2.6

pozitiiImpare_ajutor :: [Integer] -> Integer -> [Integer]
pozitiiImpare_ajutor [] _ = []
pozitiiImpare_ajutor (h:t) poz 
    | h `mod` 2 == 1 = poz:t'
    | otherwise = t'
    where t' = pozitiiImpare_ajutor t (poz+1)

pozitiiImpare :: [Integer] -> [Integer]
pozitiiImpare [] = []
pozitiiImpare xs = pozitiiImpare_ajutor xs 0

pozitiiImpareComp :: [Integer] -> [Integer]
pozitiiImpareComp xs = [ snd x | let l = zip xs [0..] , x <- l , fst(x) `mod` 2 == 1 ]

testPozitiiImpare xs = (pozitiiImpare xs) == (pozitiiImpareComp xs)

--L2.7

data Directie 
    = Nord
    | Sud
    | Est
    | Vet
  deriving (Eq,Show)

--L2.8
type Punct = (Integer,Integer)

--L2.10
type Drum = [Directie]
--L2.11
miscare_directie1 :: Directie -> Punct
miscare_directie1 Nord = (0, 1)
miscare_directie1 Sud = (0, -1)
miscare_directie1 Est = (1, 0)
miscare_directie1 Vest = (-1, 0)


