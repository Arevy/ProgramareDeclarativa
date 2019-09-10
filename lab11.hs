import Data.Monoid

data Dom a =  Empty
	    | Full
	    | Ran a a
	    | (Dom a) :|: (Dom a)
	    | (Dom a) :&: (Dom a)
	deriving Show


instance (Eq a) => Eq (Dom a) where
	(==) Full Full = True
	(==) Empty Empty = True
	(==) (Ran x1 y1) (Ran x2 y2) = x1==x2 && y1==y2
	(==) (x1 :|: y1) (x2 :|: y2) = x1==x2 && y1==y2
	(==) (x1 :&: y1) (x2 :&: y2) = x1==x2 && y1==y2

exist :: (Ord a) => a -> Dom a -> Bool
exist _ Full = True
exist _ Empty = False
exist xy (Ran x y) = (xy >= x) && (xy <= y)
exist xy (x :|: y) = (exist xy x) || (exist xy y) 
exist xy (x :&: y) = (exist xy x) && (exist xy y)

overlap :: (Ord a) => Dom a -> Dom a -> Bool
overlap Full _ = True  
overlap _ Full = True
overlap a @ (Ran x1 y1) b @ (Ran x2 y2) =  (exist x1 b) 
					|| (exist y1 b)
					|| (exist x2 a)
overlap _ _ = False

normalize :: Dom a -> Dom a
normalize Empty = Empty
normalize Full = Full
normalize a @ (Ran _ _) = a
normalize (x :|: y) = (normalize x) :|: (normalize y)
normalize (x :&: y) = 
	let x' = (normalize x) in 
	let y' = (normalize y) in 
	foldr (:|:) Empty [x1 :&: y1 | x1 <- desface x' , y1 <- desface y']

desface :: Dom a -> [Dom a]
desface (x :|: y) = (desface x) ++ (desface y)
desface x = [x]

normalize' :: Dom a -> Dom a
normalize' Empty = Empty
normalize' Full = Full
normalize' a @ (Ran _ _) = a
normalize' (a :|: b) = (normalize' a) :|: (normalize' b)
normalize' (x :&: y) = 
	let (b,aplicat) = try_normalize ((normalize' x) :&: (normalize' y)) in
	if aplicat then b else normalize' b 

try_normalize ((a :|: b) :&: c) = ((a :&: c) :|: (b :&: c),True)
try_normalize (a :&: (b :|: c)) = ((a :&: b) :|: (a :&: c),True)
try_normalize x = (x,False) 

--class Monoid a where 
--	mempty :: a
--	mappend :: a -> a -> a
--	mconcat :: [a] -> a

newtype SDom a = SDom (Dom a)
instance Monoid (SDom a) where
	mempty = SDom Empty
	mappend (SDom x) (SDom y) = SDom (x :|: y)

newtype PDom a = PDom (Dom a)
instance Monoid (PDom a) where
	mempty = PDom Full
	mappend (PDom x) (PDom y) = PDom (x :&: y)
 



