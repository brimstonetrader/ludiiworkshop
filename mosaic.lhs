
----------------------------------------------------------------
--
-- Nonogram
--
-- Random Number Generator
--
-- Standard
--
--  Chapter 7.1, Eq. 7.1.6
--  parameters from Knuth and H. W. Lewis

> inst :: Int -> Int
> inst r = ((1664525*r+1013904223) `mod` (2 ^ 32)) 

> rng :: Int -> Int -> [Int]
> rng ornd 1  = [(inst ornd)]
> rng ornd it = do 
>   let nrnd = inst ornd 
>   (nrnd : rng nrnd (it-1))

-- Has disappointingly small modular periods, but
-- an Int division of 79 seems to clean it up.

> listMod :: [Int] -> Int -> [Int]
> listMod [] _ = []
> listMod (a:as) b = ((a `div` 79) `mod` 2):(listMod as b)



-- Now with one keyboard smash, we can generate a full 
-- random noise grid.

-- ghci> (listMod (rng 687628376 100) 2)

   [1,1,0,0,0,1,0,1,0,0,
	1,0,0,1,0,1,1,0,0,1,
	1,0,1,1,0,1,1,0,1,1,
	0,1,0,0,0,0,0,1,0,1,
	1,1,0,0,0,0,1,0,1,0,
	1,1,0,1,1,0,0,1,1,1,
	1,1,1,0,1,1,0,1,0,1,
	0,1,0,1,0,1,1,1,1,1,
	0,1,1,1,0,0,1,1,0,1,
	1,0,0,1,1,1,0,1,1,1]

