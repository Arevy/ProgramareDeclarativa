in_list :: (Eq a) => a -> [a] -> Bool

in_list elem list = length ( [ x | x <- list, x == elem ] ) > 0

in_list1 :: (Eq a) => a -> [a] -> Bool
in_list1 elem (x:xs) = if (x == elem ) then
			True
		      else 
			in_list1 elem xs

in_list2 :: (Eq a) => a -> [a] -> Bool
in_list2 elem list = length ( filter (==elem) list ) > 0

zipWith1 :: (a->b->c) -> [a] ->[b]->[c]
zipWith1 _ [] _ = []
zipWith1 _ _ [] = []
zipWith1 f (x:xs) (y:ys) = f x y : zipWith f xs ys 


zipWith2 :: (a->b->c)->[a]->[b]->[c]
zipWith2 f l1 l2 = [ f x1 x2 | (x1,x2)<- zip l1 l2 ]

 
zipWith' :: (a->b->c)->[a]->[b]->[c]
zipWith' f l1 l2 =  map (uncurry f) (zip l1 l2)

is_set :: [Int] -> Bool
is_set set = length (foldl ( \acc x-> if ( in_list x acc ) then acc else (x:acc)) [] set ) == length set

is_set1 :: [Int] -> Bool
is_set1 [] = True 
is_set1 (x:xs) = if ( x `elem` xs ) then
			False
		 else 
			is_set1 xs

add_to_set :: (Eq a) => a -> [a] -> [a]
add_to_set x set = if ( x `elem` set ) then
			set 
		   else
			x:set
 
add_to_set2 :: (Eq a) => a -> [a] -> [a]
add_to_set2 x set = x:[el | el <-set, el /= x]

add_to_set3 :: (Eq a) => a -> [a] -> [a]
add_to_set3 x set = x:(filter (/=x) set )

remove_set :: (Eq a) => a-> [a] -> [a]
remove_set el list = [ x | x <- list, x /= el ]

remove_set1 :: (Eq a) => a -> [a] -> [a]
remove_set1 el set = filter (/=el) set

remove_set2 :: (Eq a) => a -> [a] -> [a]
remove_set2 _ [] = []
remove_set2 el (x:xs) = if ( el == x ) then
			   remove_set2 el xs
			else
			   x:(remove_set2 el xs)
 

union_set :: (Eq a) => [a] -> [a] -> [a]
union_set list1 list2 = list2 ++ ( filter ( not . (`elem` list2 ) ) list1)     

union_set1 :: (Eq a) => [a] -> [a] -> [a]
union_set1 list1 list2 = list2 ++ [ x | x<-list1 , not( x `elem` list2 ) ]

union_set3 :: (Eq a) => [a] -> [a] -> [a]
union_set3 [] list2 = list2
union_set3 (x:xs) list2 = if ( x `elem` list2 ) then
			  	union_set3 xs list2
			  else	
				x:(union_set3 xs list2)	

partition :: (a->Bool) -> [a] -> ([a],[a])
partition f list = ( filter (not . f) list, filter f list )

quick_sort :: (Ord a ) => [a] -> [a]
quick_sort [] = []
quick_sort list = let pivot = head list
		      (p1, p2) = partition (>pivot) ( tail list )
		  in 
			( quick_sort p1 ) ++ [ pivot ] ++ ( quick_sort p2 )

