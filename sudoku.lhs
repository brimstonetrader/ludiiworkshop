
----------------------------------------------------------------
--
-- Sudoku
--
-- Random Number Generator
--
-- Standard
--
--  Chapter 7.1, Eq. 7.1.6
--  parameters from Knuth and H. W. Lewis

> inst :: Int -> Int
> inst r = ((1664525*r+1013904223) `mod` (2 ^ 32)) 

-- 1/5th is what Tatham uses. Let's allow for user variance.

> lcg2 :: Int -> Int -> [Int]
> lcg2 _ 1  = [1]
> lcg2 r i = do 
>   let n = inst r 
>   (((n `mod` i)+1) : (lcg2 n (i-1)))

> permute :: Int -> [Int] -> [Int]
> permute seed as = do 
>   let bs = lcg2 seed (len as)
>   permute2 as bs []

> permute2 :: [Int] -> [Int] -> [Int] -> [Int]
> permute2 as []     cs = cs 
> permute2 as (b:bs) cs = do
>   let n               = len as
>   let (ds, e:es)      = splitAt (fromIntegral (b-1)) as
>   permute2 (ds++es) bs (e:cs) 

> noiseGrid :: Int -> Int -> Int -> [Int]
> noiseGrid a b seed = permute seed [1..9]

> filter :: [Int] -> [Int] -> [Int]
> filter []     ls = ls
> filter (k:ks) ls =  

-- Now with one keyboard smash, we can generate a 
-- random noise grid with a particular amount of 
-- ones.

ghci> noiseGrid 8 64 98393
	[0,0,0,1,0,0,0,0,
	 0,0,0,1,0,0,0,0,
	 0,0,0,0,0,0,0,0,
	 1,0,0,0,1,1,0,1,
	 0,0,0,0,0,0,1,0,
	 0,0,0,0,0,0,0,0,
	 0,0,0,1,0,0,0,0,
	 0,0,0,0,0,0,0,0]

> refl :: Int -> Int -> [Int] -> [Int]
> refl i n os = if i==(n*n) then [] else 
>   (os !! (fromIntegral ((n*(i `mod` n)) + (i `div` n)))):(refl (i+1) n os)

-- ghci> (refl 0 4 
				[1,1,0,0,
				 0,1,0,0,
				 0,1,1,0,
				 0,0,0,1])
				 
					->
					
				[1,0,0,0,
				 1,1,1,0,
				 0,0,1,0,
				 0,0,0,1]
				 
				 
				 
				 
				 
				 
				 
				 
				 
				 
				 
				 
				 
				 
				 

> len :: [a] -> Int
> len []     = 0
> len (a:as) = 1+(len as)

> rep :: Int -> [a] -> [a]
> rep a bs = rep2 bs a []

> rep2 :: [a] -> Int -> [a] -> [a]
> rep2 _  0 bs = bs
> rep2 as i bs = as++(rep2 as (i-1) bs)