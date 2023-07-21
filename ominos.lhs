





yk, the combnatorial argument, to this day, is the only truly convincing way I've encountered of showing that 0^0 = 1. 
The characteristic function of {0} (aka a(n) = 0^n aka A000007) looks pretty goddamn stupid, but it's hard to refute 
that there exists one mapping from an empty set onto another empty set. 

Look, what I mean to say, is really, that "Polyomino" is too long of a word for the concept of "a subset of a tiling", 
which is a concept I myself spend a great deal of time considering. I will be referring to the general class, square, 
hexagonal, even 3.4.3.4.4, as merely "Ominos". I hope this does not offend anyone.

This chart from wikipedia rocks

 n    name        free    fixed
-------------------------------
 1 	monomino 	1 	 	1
 2 	domino 	    1 	 	2
 3 	tromino 	2 	 	6
 4 	tetromino 	5 	 	19
 5 	pentomino 	12 	 	63
 6 	hexomino 	35 	 	216
 7 	heptomino 	108 	760
 8 	octomino 	369 	2725
 9 	nonomino 	1285 	9910
 10 decomino 	4655 	36446
 11 undecomino 	17073 	135268
 12 dodecomino 	63600 	505861

We can think of an omino as a square list of lists of booleans, like

O      [1,0,0]
OO ->  [1,1,0]
O      [1,0,0]

We can have a single list, instead, that contains n^2 booleans, or even a n^2-bit number, but that might be extraneous. 
Let's say that a "well-formed" polyomino representation in this system always has at least one True in the top row, and
left-most column. With this in mind, we can convert the above sequence to a number like so

[1,0,0] -> [1,   2,  4]
[1,1,0] -> [8,  16, 32] -> 89
[1,0,0] -> [64,128,256]

This is actually bad, though, because if we're faced with this same polyomino on a 5x5, or worse, nxn grid, its number 
will have changed, meaning we know jack squat. A better method, which assigns a unique number to every polyomino no 
matter the grid size, would be more like this.

[1,0,0] -> [  1,  2,16]
[1,1,0] -> [  8,  4,32] -> 269
[1,0,0] -> [256,128,64]

This means that every natural number bijects to a particular form in the grid, and the grid size does not change this 
number. For a Form to be an Omino, in this notation, it must be

	- CONTIGUOUS (there exists a path crossing only adjacent grid cells from any cell in the omino to 
	              any other cell in the omino)
	  
	- MINIMAL (there exists a cell in the omino along the top row and leftmost column. Thus, this is 
			   the smallest number that can represent it in this system. If we do away with this one,
			   we have an infinite family of representations for any omino.)



> convert :: Int -> [Bool]
> convert 0 = []
> convert n = do 
>   let (nd,nm) = n `divMod` 2
>   if nm == 1 then True:(convert nd) else False:(convert nd)


> rearrange :: [a] -> [a]
> rearrange ns = do 
>   let l  = len ns
>   let r  = sq l
>   let k = r*r-l
>   rearrange2 1 1 r (ns++(rep k 0)) [[]]

> rearrange2 :: Int -> Int -> Int -> [a] -> [[a]] [a]
> rearrange2 i j r os ns = if j>r then ns else if i>(2*j-1) 
>   then do 
>     let (as,bs) = splitAt ((len ns) - 1) ns
>     rearrange2 1 (j+1) r os (as++reverse bs) 
>   else if i<j then else rearrange2 (i+1) j 
>     

 showOmino :: [Bool] -> IO ()
 showOmino bs = do 
   let n = sq bs
   

-- A FREE Omino is still the same regardless of how its reflected or rotated. A FIXED Omino can be changed by 
-- either of those operations, but not reliably. For example, 

--  O
-- OOO is the same fixed Omino regardless of how you reflect or rotate it. Each fixed Omino can be bijected to a 
--  O  sequence of TURTLE STEPS, which are as follows.

> data Step1 where
>   Forward :: Step1
>   Lef     :: Step1
>   Righ    :: Step1

-- The free tetrominoes, better known as the cast of Tetris.

-- O     O            O      O
-- O     O     OO     OO     OO
-- O  ,  OO  , OO   ,  O  ,  O 
-- O       


> tOne   :: [Step1]
> tTwo   :: [Step1]
> tThree :: [Step1]
> tFour  :: [Step1]
> tFive  :: [Step1]

> tOne   = [Forward,                    Forward,        Forward]
> tTwo   = [Forward,                    Forward,   Lef, Forward]
> tThree = [Forward,               Lef, Forward,   Lef, Forward]
> tFour  = [Forward,               Lef, Forward,  Righ, Forward]
> tFive  = [Forward, Forward, Lef, Lef, Forward,   Lef, Forward]



-- A pattern that crops up often as we go higher than n=4 is "Forward, Left, Left, Forward," which only adds one new unit 
-- to our omino. Let's write a new ADT, shortening all to single letters, and calling this new thing a "loop", as it brings 
-- one back where they started. "Loop", unfortunately, begins with the same letter as "Left", so we'll call it "O" for 
-- obvious reasons.

> data Step where
>   F :: Step
>   L :: Step
>   R :: Step
>   O :: Step
                                                                                
   1                      
   1       22                     5        
   1        2     333      44     55       666 
   1        2       3      44      5        6 
   1     ,  2  ,    3   ,   4  ,   5    ,   6
                                                   
   7                                                 
  77              9       AA       BB       C              
   7       888    999      AA     BB       CCC                
   7     , 8 8 ,    9   ,   A  ,   B    ,   C                                           
                         
> p1  = [F,   F,   F,   F]
> p2  = [F,   F,   F,L, F]
> p3  = [F,   F,L, F,   F]
> p4  = [F,   F,L, F,L, F]
> p5  = [F,   F,L, F,R, F]
> p6  = [F,   F,L, O, , F]
> p7  = [F,   F,L, O,L, F]
> p8  = [F,L, F,   F,L, F]
> p9  = [F,L, F,   F,R, F]
> p10 = [F,L, F,R, F,L, F]
> p11 = [F,L, O,L, F,R, F]
> p12 = [F,L, O,L, O,L, F]

We can arbitrarily sort them by this ranking: 

[F  ->  F,L  ->  F,R  ->  O  ->  O,L  ->  O,R]

Reversing a list    of turtle steps creates the same polyomino, but rotated 180 degrees. 

Prefacing a list    of turtle steps with x Ls rotates an Omino by x*90 degrees

Replacing instances of L with R and vice versa reflects a polyomino about the y-axis.

ominoes in square-space are beholden to the symmetry group D_8, which has three generators 
that we'll concern ourselves with, r, rrs, and s. In group theory, in a group of form D_X, 
"r" means to rotate by 360/(X/2) degrees, and "s" means to reflect about the y-axis. "e"
means just regular style. Groups of form "D_X" catalog all possible symmetries of regular 
polygons. Below, a chart.

_|   e, r, rr, rrr, s, rs, rrs, rrrs  
1    1  0   1    0  1   0    1     0
2    1  0   0    0  0   0    0     0
3    1  0   0    0  0   1    0     0
4    1  0   0    0  0   0    0     0
5    1  0   0    0  0   0    0     0
6    1  0   0    0  1   0    0     0
7    1  0   0    0  0   0    0     0
8    1  0   0    0  1   0    0     0
9    1  0   1    0  0   0    0     0
A    1  0   0    0  0   1    0     0
B    1  0   0    0  0   0    0     0
C    1  1   1    1  1   1    1     1


one can plainly glean that the important bits are rr, s, and rs. They "generate" our other symmetries.
if we verify that a polyomino has or does not have some combination of the three, we can fill in the 
rest of the table. It's sort of like vertices on a cube, like 


     rs   -  r
   /  |      |
- e  rrrs -  rrr
  | /       /
  rr  - rrs

who's hiding? 


being symmetric about s means y=0 is your line of symmetry. rr means y=x is, and rs means y=-x.















[]

> printOmino :: Char -> [[Bool]] -> String
> printOmino a ((b:[]):[])  = if b then [a] ++ "\n"                  else "\n"  
> printOmino a ([]:bss)     = "\n" ++ printOmino a bss   
> printOmino a ((b:bs):[])  = if b then a:printOmino a (bs:[])       else ' ':printOmino a (bs:[])  
> printOmino a ((b:bs):bss) = if b then a:(printOmino a (bs:bss))    else ' ':printOmino a (bs:bss)  

-- ghci> putStr (printOmino 'X' [[False,True,False],[True,True,True],[True,False,False]])
--  X
-- XXX
-- X

 split :: Int -> Int -> [[Bool]] -> [[Bool]]
 split x y grid = do 
   



> square :: Int -> [a] -> [[a]]
> square s [] = [] 
> square s as = do 
>   let (bs,cs) = splitAt s as
>   bs:(square s cs)
 
> monomino :: Int -> [[Bool]]
> monomino n = init (emptyGrid2 0 n )

> emptyGrid2 :: Int -> Int -> [[Bool]]
> emptyGrid2 i n = if i==n-1 
>   then (True:(rep (n-1) False)):(emptyGrid2 (i+1) n) 
>   else if i<n 
>     then (rep n False):(emptyGrid2 (i+1) n) 
>     else [[]]




 randOmino :: Int -> Int -> IO ()
 randOmino seed n = do 
   let rnds = (boolNoise seed 1000)
   putStr (printOmino (square n rnds))


> rep :: Int -> a -> [a]
> rep 0 b = []
> rep a b = b:(rep (a-1) b)

> sq :: Int -> Int
> sq n = sqrt2 0 n

> sqrt2 :: Int -> Int -> Int
> sqrt2 x y = if (x*x)<y then sqrt2 (x+1) y else x

> maxlength :: [[a]] -> Int
> maxlength [[]] = 0
> maxlength [as] = len as
> maxlength (as:bs:css) = do 
>   let la = len as
>   let lb = len bs
>   if (la>lb) then maxlength (as:css) else maxlength (bs:css) 

> len :: [a] -> Int
> len []     = 0
> len (a:as) = 1+(len as)




-- ghci> printGrid 10

	_ _ _ _ _ _ _ _ _ _
   |_|_|_|_|_|_|_|_|_|_|
   |_|_|_|_|_|_|_|_|_|_|
   |_|_|_|_|_|_|_|_|_|_|
   |_|_|_|_|_|_|_|_|_|_|
   |_|_|_|_|_|_|_|_|_|_|
   |_|_|_|_|_|_|_|_|_|_|
   |_|_|_|_|_|_|_|_|_|_|
   |_|_|_|_|_|_|_|_|_|_|
   |_|_|_|_|_|_|_|_|_|_|
   |_|_|_|_|_|_|_|_|_|_|
   

> inst :: Int -> Int
> inst r = ((1664525*r+1013904223) `mod` (2 ^ 32)) 

> lcg :: Int -> Int -> [Int]
> lcg ornd 1  = [(inst ornd)]
> lcg ornd it = do 
>   let nrnd = inst ornd 
>   (nrnd : lcg nrnd (it-1))

> listMod :: [Int] -> Int -> [Int]
> listMod [] _ = []
> listMod (a:as) b = ((a `div` 79) `mod` 2):(listMod as b)

 boolNoise :: Int -> [Bool]
 boolNoise n c = do 
   let xs = (listMod (lcg n c) 2)
   i2b xs

> i2b :: [Int] -> [Bool]
> i2b []     = []
> i2b (0:is) = False:(i2b is)
> i2b (1:is) =  True:(i2b is)
> i2b (c:is) =       (i2b ((c `mod` 2):is))