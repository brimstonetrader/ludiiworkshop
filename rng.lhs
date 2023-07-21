-- Linear Congruential Generator
--
--Chapter 7.1, Eq. 7.1.6
--parameters from Knuth and H. W. Lewis
--
--This is like the default RNG. Java used it
--up until 2020.
--
--The basic equation is x_2 = (a*x + c) % m.
-- 
--  Make m as big as possible, and ensure it's 
--  got no factors other than two.
--
--    Make c prime, ideally, or at the very least
--    coprime to m.
--
--    Then, if you ensure these two things, it'll
--    only repeat once every m times:
--
--      1. a-1 `div` f == 0 for all prime factors f of m
--      2. a-1 `mod` 4 == 1 if m `div` 4 == 0

> inst :: Int -> Int
> inst r = ((1664525*r+1013904223) `mod` (2 ^ 32)) 

> lcg :: Int -> Int -> [Int]
> lcg ornd 1  = [(inst ornd)]
> lcg ornd it = do 
>   let nrnd = inst ornd 
>   (nrnd : lcg nrnd (it-1))

-- Middle Square Method
-- 
-- John Von Neumann, 1949
--
-- You rip out the middle of the input number after squaring it.
--
-- Silly bad. Not a good one to use. 


> lenn :: Int -> Int
> lenn i = if i<1 then 0 else 1 + (lenn (i `div` 10))

> inst2 :: Int -> Int
> inst2 0        =  0
> inst2 r        =  do
>   let rr       =  r*r
>   let (lr,lrr) = (lenn r,lenn rr)
>   let x        = (lrr - lr) `div` 2
>   (rr `div` (10^x)) `mod` (10^lr)

> msm :: Int -> Int -> [Int]
> msm ornd 1 = [(inst2 ornd)]
> msm ornd it = do 
>   let nrnd = inst2 ornd 
>   if nrnd == ornd then [ornd] else (nrnd : msm nrnd (it-1))

-- Lehmer RNG 
--
-- LCG with one term removed. Better than MSM.
-- The two numbers should be coprime.

> inst3 :: Int -> Int
> inst3 r = (16807*r) `mod` ((2 ^ 31)-1) 

> lrng :: Int -> Int -> [Int]
> lrng ornd 1  = [(inst3 ornd)]
> lrng ornd it = do 
>   let nrnd = inst3 ornd 
>   (nrnd : lrng nrnd (it-1))


-- Lagged Fibonacci Generator
--
-- x_n = (x_(n-j) <op> x_(n-k)) % m
--
-- m is usually 2^2^x where x is something or other.
-- 
-- y = x^k + x^j + 1 needs to be irreducible (unfactorable)
-- and primitive mod 2
--
-- page 29 of volume 2 of The Art of Computer Programming:

-- (24, 55),  (38, 89),   (37, 100),  (30, 127), 
-- (83, 258), (107, 378), (273, 607), (1029, 2281), 
-- (576, 3217), (4187, 9689), (7083, 19937), (9739, 23209)
--
-- (5,17) (6,31) and (7,10) also work.
--
-- <op> is any binary operation, but xor works best.
-- Why does haskell not have bitwise operations
-- in the Prelude smh my head 

> bxor :: Int -> Int -> Int
> bxor a 0 = a
> bxor 0 b = b
> bxor a b = do
>   let (da,ma) = a `divMod` 2
>   let (db,mb) = b `divMod` 2
>   if ma == mb 
>     then 2*(bxor da db)
>     else 1+(2*(bxor da db))

> lfg :: Int -> Int -> [Int]
> lfg ornd it = lfg2 (lcg ornd 55) it

> lfg2 :: [Int] -> Int -> [Int] 
> lfg2 seeds 0 = []
> lfg2 seeds i = do 
>   let nrnd   = (bxor (seeds !! ((len seeds)-24)) (head seeds)) `mod` (2^32)
>   (nrnd : lfg2 (nrnd : take 54 seeds) (i-1))

-- Rule 30
-- 
-- 30 in base 10 = 00011110 in base 2
-- 
-- 111 110 101 100 011 010 001 000
--  0   0   0   1   1   1   1   0
--
-- We can make a 1d cellular automaton here,
-- and convert each new state into a number.

> inst4 :: Int -> Int
> inst4 0 = 0
> inst4 r = do 
>   let w = r `mod` 8 
>   if 0 < w && w < 5
>     then 1 + (2*(inst4 (r `div` 2)))
>     else     (2*(inst4 (r `div` 2)))


> rule30 :: Int -> Int -> [Int]
> rule30 ornd 1  = [(inst4 ornd)]
> rule30 ornd it = do 
>   let nrnd = inst4 ornd 
>   (nrnd : lcg nrnd (it-1))






> extract :: Int -> [a] -> [a] 
> extract 0 as     = []
> extract n []     = []
> extract n (a:as) = a:(extract (n-1) as)

> retract :: Int -> [a] -> [a] 
> retract 0 as     = as
> retract n []     = []
> retract n (a:as) = retract (n-1) as


> len :: [a] -> Int
> len []     = 0
> len (a:as) = 1+(len as)

TODO:
	INVERSIVE CONGRUENTIAL GENERATOR
	BLUM BLUM SHUB
	ACORN
	MIXMAX
	KISS
	MERSENNE TWISTER
	XORSHIFT
	LINEAR-FEEDBACK SHIFT REGISTER