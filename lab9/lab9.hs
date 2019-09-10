-- Informatics 1 - Functional Programming 
-- Tutorial 9
--


import LSystem
import Test.QuickCheck

-- Exercise 1

-- 1a. split
split :: Command -> [Command]
split ( com1 :#: rest ) = ( split com1 ) ++ ( split rest ) 
split Sit = []
split cmd = [cmd]

-- 1b. join
join :: [Command] -> Command
join [] = Sit
join (hd:tl) = hd :#: ( join tl ) 

-- 1c  equivalent
equivalent :: Command -> Command -> Bool
equivalent cmd1 cmd2 = (split cmd1 ) == (split cmd2) 

-- 1d. testing join and split
-- prop_split_join com = join ( split com ) == c 

-- prop_split split_reslut = ( not (Sit `elem` split_reslut )  


-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy times cmd = join ( replicate times cmd ) 

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon dist = copy 5 ( Go dist :#: Turn 72 )

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon dist nr = copy nr ( Go dist :#: Turn ( 360 / (fromIntegral nr) ) )



-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral _ 0 _ _ = Sit
spiral segm n step angle = Go segm :#: Turn angle :#: ( spiral (segm + step) (n-1) step angle ) 


-- Exercise 4
-- optimise
optimise :: Command -> Command
optimise = undefined



-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead = undefined

-- 6. snowflake
snowflake :: Int -> Command
snowflake = undefined

-- 7. hilbert
hilbert :: Int -> Command
hilbert = undefined

main :: IO ()
main = display (Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#: Go 30)
