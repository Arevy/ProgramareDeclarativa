-- Informatics 1 Functional Programming
-- Tutorial 8
--

import System.Random


-- Importing the keymap module
import Data.List
import KeymapTree


-- Type declarations

type Barcode = String
type Product = String
type Unit    = String

type Item    = (Product,Unit)

type Catalogue = Keymap Barcode Item


-- A little test catalog

testDB :: Catalogue
testDB = fromList [
 ("0265090316581", ("The Macannihav'nmor Highland Single Malt", "75ml bottle")),
 ("0903900739533", ("Bagpipes of Glory", "6-CD Box")),
 ("9780201342758", ("Thompson - \"Haskell: The Craft of Functional Programming\"", "Book")),
 ("0042400212509", ("Universal deep-frying pan", "pc"))
 ]


-- Exercise 1

longestProductLen :: [(Barcode, Item)] -> Int
longestProductLen list = maximum ( map (length.snd) list ) 

formatLine :: Int -> (Barcode, Item) -> String
formatLine des_len (barcode, (product, unit) ) = barcode ++ "..." ++ product ++ ( replicate ( des_len - (length product) + 3) '.' ) ++ unit

showCatalogue :: Catalogue -> String
showCatalogue database = let dbList = toList database
                             maxLen = length dbList + 5
                         in intercalate "\n" (map (formatLine maxLen)  dbList )
     
-- Exercise 2
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (hd:_) = Just hd 

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Nothing):tl) = catMaybes tl 
catMaybes ((Just val):tl) = val:(catMaybes tl)

-- Exercise 3

getItems :: [Barcode] -> Catalogue -> [Item]
getItems barcodes catalog = catMaybes ( map (\barcode -> get barcode catalog ) barcodes)

-- Input-output ------------------------------------------

readDB :: IO Catalogue
readDB = do dbl <- readFile "database.csv"
            let db = fromList (map readLine $ lines dbl)
            putStrLn (size db >= 0 `seq` "Done")
            return db

readLine :: String -> (Barcode,Item)
readLine str = (a,(c,b))
    where   
      (a,str2) = splitUpon ',' str
      (b,c)    = splitUpon ',' str2

splitUpon :: Char -> String -> (String,String)
splitUpon _ "" = ("","")
splitUpon c (x:xs) | x == c    = ("",xs)
                   | otherwise = (x:ys,zs)
                   where
                     (ys,zs) = splitUpon c xs

getSample :: Catalogue -> IO Barcode
getSample db = do g <- newStdGen
                  return $ fst $ toList db !! fst (randomR (0,size db - 1) g)