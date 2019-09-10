import Data.Char
import Test.QuickCheck
 
type Cifra = Int
type Numar = [Cifra]

--ex1a
lungimePlus :: Numar -> Int -> Numar
lungimePlus [] 0 = []
lungimePlus lista 0 = lista
lungimePlus (l:tl) nr = 0 : lungimePlus (l:tl) ((nr)-1) 
                            
--ex1b

normalizeazaLungime :: (Numar, Numar) -> (Numar, Numar)
normalizeazaLungime ([],[]) = ([],[])
normalizeazaLungime ([],lista2) = (lungimePlus [] (length lista2) , lista2)
normalizeazaLungime (lista1, []) = (lista1 , lungimePlus [] (length lista1))
normalizeazaLungime (lista1,lista2) =
    if length lista1 > length lista2
        then (lista1,lungimePlus lista2 ((length lista1) -(length lista2)))
        else (lungimePlus lista1 ((length lista2) -(length lista1)) , lista2)

--ex2a
lteN :: Numar -> Numar -> Bool
lteN [] [] = True
lteN lista [] = False
lteN [] lista = False
lteN lista1 lista2 = 
    if lista1 <= lista2
        then True
        else False
--ex2b
lte :: Numar -> Numar -> Bool
lte [] [] = True
lte lista [] = False
lte [] lista = True
lte lista1 lista2 =  
    if fst(normalizeazaLungime(lista1,lista2)) <= snd (normalizeazaLungime(lista1,lista2))
        then True
        else False

--ex3a
numar :: Numar -> Int
numar nr = (foldr op unit nr ) 0
	where
		unit :: Int -> Int 
		unit suma = suma

		op :: Cifra -> (Int -> Int) -> (Int -> Int)
		(op a rest) suma = rest (suma * 10 + a)

mulC :: Cifra -> Numar -> Numar
mulC c nr = dropWhile (== 0) (reverse(mulAux c (reverse nr)))

mulAux :: Cifra -> Numar -> Numar 
mulAux c nr = (foldr op unit nr) 0
	where 
		unit :: Int -> Numar 
		unit r = [r]

		op :: Cifra -> (Int -> Numar) -> (Int -> Numar)
		op a eq rest
			| (a * c + rest) > 9 = mod (a*c + rest) 10 : eq (div (a*c + rest) 10)
			| otherwise = a * c + rest : eq 0
	
norm :: Numar -> Int -> Numar 
norm a nr = (replicate nr 0) ++ a

plus :: Numar -> Numar -> Numar
plus a b
	| length a > length b = dropWhile (== 0) (reverse $ plusFunc $ plusAux(a, (norm b (length a - length b))))
	| length a < length b = dropWhile (== 0) (reverse $ plusFunc $ plusAux((norm a (length b - length a)), b))
	| otherwise = dropWhile (== 0) (reverse $ plusFunc $ plusAux (a, b))

plusAux :: (Numar, Numar) -> [(Cifra, Cifra)]
plusAux (a, b) = zip (reverse a) (reverse b)

plusFunc :: [(Cifra, Cifra)] -> Numar
plusFunc arr = (foldr op unit arr) 0
	where 
		unit :: Int -> Numar 
		unit r = [r]

		op :: (Cifra, Cifra) -> (Int -> Numar) -> (Int -> Numar)
		op a eq rest 
			| (fst a + snd a + rest) > 9 = mod (fst a + snd a + rest) 10 : eq (div (fst a + snd a + rest) 10)
			|  otherwise = fst a + snd a + rest : eq 0
  
        