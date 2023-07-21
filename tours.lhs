A grid is a singly linked list which looks like this

[0, 1, 2, 3, 4,
 5, 6, 7, 8, 9,
10,11,12,13,14,
15,16,17,18,19,
20,21,22,23,24]

You can figure out who's in your (row,column) with Euclidean Division.

A Move from one unit to another consists of a positive or negative number of shifts in x and y. We can represent this as a Vector, or ordered pair.


> data Move where
>   Sing :: Vector -> Sym -> Limit -> Move
>   List :: [Move] -> Move

> data Limit where
>   Num   :: Int        -> Limit
>   Range :: (Int, Int) -> Limit
>   Any   :: [Int]      -> Limit
>   No    ::               Limit

> data Sym where
>   None :: Sym 
>   Line :: Sym 
>   Quad :: Sym 
>   Octo :: Sym 

> type Vector = (Int, Int)

we can think of each number representing a movement representable by a tuple of numbers. 
Diagonal moves look like (x,x), Orthogonal moves look like (0,x) or (x,0). Hippogonal 
moves look like (x,y). Here are the chess pieces with this abstract syntax.


> pMove = Sing (0,1) None (Num 1)
> rMove = Sing (0,1) Quad  No
> nMove = Sing (1,2) Octo (Num 1)
> bMove = Sing (1,1) Quad  No
> qMove = Sing (0,1) Octo  No
> kMove = Sing (0,1) Octo (Num 1)


with that in mind, we can permute a list of integers to get something akin to a 
"knight's tour". This is a sequence of knight moves on a chess-board that traverse 
the entire board without stopping on the same square twice. Since this is truly random,
this is a teleporting knight's tour.

> rpt :: Int -> Int -> [Int]
> rpt seed n = do 
>   let rnds = permute seed [2..(n*n-1)]
>   ((1:rnds)++[n*n])

ghci> rpt 13123213 5
	
	[1,13,20,4,9,2,11,21,14,12,16,3,18,7,19,22,10,8,6,5,15,17,24,23,25]

it's notable that there are two valid ways of reading these: as a sequence
of addresses, or as a sequence of times. That didn't make much sense at all.
Consider the n=3 case.

ghci> permute 890410 [1..9]
	[7,3,8,1,9,5,2,6,4]
	
We take the initial seven to mean that 1 is visited on the seventh moment of the tour.
The 3 in place 2 means that three is visited second, and so on. Thus, we want

	f([7,3,8,1,9,5,2,6,4]) = 
	  [4,7,2,9,6,8,1,3,5]

> switch :: [Int] -> [Int]
> switch ns = switch2 1 (len ns) ns

> switch2 :: Int -> Int -> [Int] -> [Int]
> switch2 i m ns = if i>m then [] else (1+whereIs i ns):(switch2 (i+1) m ns)

ghci> switch [1,13,20, 4, 9, 2,11,21,14,12,16, 3,18, 7,19,22,10, 8, 6, 5,15,17,24,23,25]
			 [1, 6,12, 4,20,19,14,18, 5,17, 7,10, 2, 9,21,11,22,13,15, 3, 8,16,24,23,25]

This is an interesting operation to do on a permutation because it is entirely reversible. Look,

ghci> switch [1, 6,12, 4,20,19,14,18, 5,17, 7,10, 2, 9,21,11,22,13,15, 3, 8,16,24,23,25]
			 [1,13,20, 4, 9, 2,11,21,14,12,16, 3,18, 7,19,22,10, 8, 6, 5,15,17,24,23,25]
			 
This inclines me to believe that "switching" a permutation allows us to find something like its inverse.
We can now measure a permutation in terms of its cycles, like


	[3,1,5,2,6,4]  2 <-- 3  1 <-- 5   -This is written (145)(236)
	[2,4,1,6,3,5]  \> 6 -^  \> 4 -^   

From these two permutations, we can write two more, like

	[3,1,5,2,6,4] at index [2,4,1,6,3,5] -> [5,3,6,1,4,2]
	[2,4,1,6,3,5] at index [3,1,5,2,6,4] -> [4,6,2,5,1,3]
	
This operation is fun. For any permutation, [x,y,z...] at index [x,y,z...] -> [1,2,3...].
So any permutation, under this operation, is its own inverse. Additionally, for any permutation,
[x,y,z...] at index [1,2,3...] -> [x,y,z...], and [1,2,3...] at index [x,y,z...] -> [x,y,z...]
	
> (@) :: [Int] -> [Int] -> [Int] 
> as @ bs = do 
>   let (la,lb) = (len as, len bs) 
>   atIndex (lb-la) la as bs
	
> atIndex :: Int -> Int -> [Int] -> [Int] -> [Int]
> atIndex i n as bs = if i==n then [] else (as !! (whereIs (i+1) bs)):(atIndex (i+1) n as bs) 
	
Our loop structure is the same - the values are all the same. There are two loops containing
three values each, [2,3,6] and [1,4,5]. In general, a @ (switch a) = reverse (switch a). 

ghci> [3,1,5,2,6,4] @ [2,4,1,6,3,5]
									[5,3,6,1,4,2]
ghci> [2,4,1,6,3,5] @ [3,1,5,2,6,4]
									[4,6,2,5,1,3]
ghci> [3,1,5,2,6,4] @ [1,2,3,4,5,6]
									[3,1,5,2,6,4]                 
ghci> [5,3,6,1,4,2] @ [6,5,4,3,2,1]
									[2,4,1,6,3,5]	

This operation is not abelian. a @ b does not equal b @ a. Here it is on two permutations that are 
not related, with different cycles. 
		
ghci> [5,2,4,3,6,1] @ [4,1,2,6,3,5]
									[2,4,6,5,1,3]
ghci> [4,1,2,6,3,5] @ [5,2,4,3,6,1]
									[5,1,6,2,4,3]
		

All of these permutations are isomorphic under S_6, representable as (145)(236) = (14)(45)(51)(23)(36)(62). 


	
> randPathTelep :: Int -> Int -> IO ()
> randPathTelep seed n = do 
>   let rnds = permute seed [2..(n*n-1)]
>   printTour n ((1:rnds)++[n*n])

> printTour :: Int -> [Int] -> IO ()
> printTour _ [] = putStr "\n"
> printTour n as = if n>len as then putStrLn (show as) else do 
>   let (bs,cs) = splitAt n as
>   putStrLn (show bs)
>   printTour n cs

We can convert this into a list of vectors.

> catalog :: [Int] -> [Vector]
> catalog ns = catalog2 (sq (len ns)) ns  

> catalog2 :: Int -> [Int] -> [Vector]
> catalog2 _ [a]      = []
> catalog2 n (a:b:cs) = do 
>   let (dr,dc) = (b%n - a%n,a//n - b//n)
>   let r = if a%n  - dc < -1 then dr+n else if a%n  - dc >= n then dr-n else dr
>   let c = if a//n - dr < -1 then dc+n else if a//n - dr >= n then dc-n else dc 
>   (r,c):(catalog2 n (b:cs)) 

ghci> randPathTelep 234423324 3
  [1,7,6]
  [8,4,5]
  [2,3,9]

ghci> catalog (switch [1,7,6,8,4,5,2,3,9])
  [(0,-2),(1,0),(0,1),(-2,-1),(0,1),(2,1),(-1,-1),(-1,-2)]

ghci> printTour 5 [1,18,11,20,7,12,3,23,21,17,22,9,14,2,13,15,24,6,10,5,4,19,16,8,25]

  [ 1,18,11,20,7]
  [12, 3,23,21,17]
  [22, 9,14, 2,13]
  [15,24, 6,10,5]
  [ 4,19,16, 8,25]
  
ghci> catalog (switch   [ 1,18,11,20,7]
						[12, 3,23,21,17]
						[22, 9,14, 2,13]
						[15,24, 6,10,5]
						[ 4,19,16, 8,25])

  [(3,-2),(-2,1),(-1,-3),(-1,0),(3,1),
   (-3,2),(4,-3),(-2,2),(2,-1),(-1,3),
   (-2,-1),(-1,-2),(3,1),(-2,-1),(2,-1),
   (-3,2),(2,2),(0,-4),(2,4),(0,-1),
   (-3,-1),(2,1),(-1,-2),(-2,-2)]
 
There's a fascinating issue here - these vectors work perfectly, provided you embed 
your grid to a torus.












-- HELPERS

> intToChar :: Int -> Char
> intToChar 1  = '0'
> intToChar 2  = '1'
> intToChar 3  = '2'
> intToChar 4  = '3'
> intToChar 5  = '4'
> intToChar 6  = '5'
> intToChar 7  = '6'
> intToChar 8  = '7'
> intToChar 9  = '8'
> intToChar 10 = '9'
> intToChar 11 = 'A'
> intToChar 12 = 'B'
> intToChar 13 = 'C'
> intToChar 14 = 'D'
> intToChar 15 = 'E'
> intToChar 16 = 'F'
> intToChar 17 = 'G'
> intToChar 18 = 'H'
> intToChar 19 = 'I'
> intToChar 20 = 'J'
> intToChar 21 = 'K'
> intToChar 22 = 'L'
> intToChar 23 = 'M'
> intToChar 24 = 'N'
> intToChar 25 = 'O'
> intToChar 26 = 'P'
> intToChar 27 = 'Q'
> intToChar 28 = 'R'
> intToChar 29 = 'S'
> intToChar 30 = 'T'
> intToChar 31 = 'U'
> intToChar 32 = 'V'
> intToChar 33 = 'W'
> intToChar 34 = 'X'
> intToChar 35 = 'Y'
> intToChar 36 = 'Z'
> intToChar x  = intToChar (x%16)

> convert :: Int -> Int -> [Int] -> String
> convert i n [] = ""
> convert i n (r:rnds) = if (i-3)%n==0 
>   then ((intToChar r):"\n") ++ (convert (i+1) n rnds) 
>   else (intToChar r):(convert (i+1) n rnds)

-- THE SECTION I COPY/PASTE INTO EVERY NEW HASKELL DOCUMENT
-- DO NOT READ UNLESS YOU ARE A COMPUTER

> len :: [a] -> Int
> len []     = 0
> len (a:as) = 1+(len as)

> rep :: Int -> a -> [a]
> rep 0 b = []
> rep a b = b:(rep (a-1) b)

> amanybs :: Int -> a -> [a]
> amanybs 0 b = []
> amanybs a b = (b:amanybs (a-1) b)

> (%) :: Int -> Int -> Int
> a % b = a `mod` b

> (//) :: Int -> Int -> Int
> a // b = a `div` b

> (/%/) :: Int -> Int -> (Int, Int)
> a /%/ b = a `divMod` b

> (~) :: Int -> Int -> Int
> a ~ 0 = a
> 0 ~ b = b
> a ~ b = do
>   let (da,ma) = a /%/ 2
>   let (db,mb) = b /%/ 2
>   if ma == mb 
>     then    2*(da ~ db)
>     else 1+(2*(da ~ db))

> sort :: [Int] -> [Int]
> sort      [] = []
> sort     [x] = [x]
> sort   [x,y] = case (x>y) of
>      False -> [x,y]
>      True  -> [y,x]  
> sort list    = do
>   let (as, b:bs)  = splitAt ((len list) `div` 2) list
>   let [cs, ds]    = pivotAbout b as [[],[]]
>   let [es, fs]    = pivotAbout b bs [[],[]]
>   (sort (cs++es)) ++ (b:(sort (ds++fs)))

> pivotAbout :: Int -> [Int] -> [[Int]] -> [[Int]]
> pivotAbout n []     [a,b]  = [a,b]
> pivotAbout n [x]    [a,b]  = if (x<n) 
>                          then [x:a,b] 
>                          else [a,x:b]
> pivotAbout n (x:xs) [a,b]  = if (x<n) 
>                          then pivotAbout n xs [x:a,b] 
>                          else pivotAbout n xs [a,x:b]

> inst :: Int -> Int
> inst r = ((1664525*r+1013904223) `mod` (2 ^ 32)) 

> lcg :: Int -> Int -> [Int]
> lcg ornd 1  = [(inst ornd)]
> lcg ornd it = do 
>   let nrnd = inst ornd 
>   (nrnd : lcg nrnd (it-1))

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
>   let n          = len as
>   let (ds, e:es) = splitAt (fromIntegral (b-1)) as
>   permute2 (ds++es) bs (e:cs) 

> removeRepeats :: [Int] -> [Int]
> removeRepeats [] = []
> removeRepeats (a:as) = if a `elem` as then removeRepeats as else a:(removeRepeats as)

> intersection :: [Int] -> [Int] -> [Int]
> intersection []     bs = []
> intersection (a:as) bs = if a `elem` bs then a:(intersection as bs) else intersection as bs

> union        :: [Int] -> [Int] -> [Int]
> union as bs = removeRepeats (as ++ bs)

> difference :: [Int] -> [Int] -> [Int]
> difference as bs = sort (difference2 as (intersection as bs))

> difference2 :: [Int] -> [Int] -> [Int]
> difference2 []     bs = []
> difference2 (a:as) bs = if a `elem` bs then difference2 as bs else a:(difference2 as bs)

> disjunction :: [Int] -> [Int] -> [Int]
> disjunction as bs = difference (union as bs) (intersection as bs)

> refl :: Int -> Int -> [Int] -> [Int]
> refl i n os = if i==(n*n) then [] else 
>   (os !! (fromIntegral ((n*(i `mod` n)) + (i `div` n)))):(refl (i+1) n os)

> isPrime :: Int -> Bool 
> isPrime p = if p `mod` 2 == 0 then False 
>               else isPrime2 p 3 (sq p) 

> isPrime2 :: Int -> Int -> Int -> Bool 
> isPrime2 p i n = if i>n  then True 
>   else if p `mod` i == 0 then False 
>                          else isPrime2 p (i+2) n

> nextPrime :: Int -> Int  
> nextPrime n = if isPrime n then n else nextPrime (n+1)

> sq :: Int -> Int
> sq n = sqrt2 0 n

> sqrt2 :: Int -> Int -> Int
> sqrt2 x y = if (x*x)<y then sqrt2 (x+1) y else x

> ack :: Int -> Int -> Int
> ack 0 n = n+1
> ack m 0 = ack (m-1) 1
> ack m n = ack (m-1) (ack m (n-1))

> factorial :: Int -> Int -> Int
> factorial 0 k = k
> factorial n k = factorial (n-1) (k*n)

> choose :: Int -> Int -> Int
> choose n k = (factorial n 1) `div` ((factorial (n-k) 1)*(factorial k 1))

> fall :: Int -> Int -> Int
> fall n k = (factorial n 1) `div` (factorial (n-k) 1)

> stir :: Int -> Int -> Int
> stir n k =    if n==k then 1 
>   else if k==0 || k>n then 0 
>   else (k*(stir (n-1) k)) + (stir (n-1) (k-1)) 

> whereIs :: Int -> [Int] -> Int
> whereIs a bs = wI2 a bs 0

> wI2 :: Int -> [Int] -> Int -> Int
> wI2 a []     c = -1
> wI2 a (b:bs) c = if a==b then c else wI2 a bs (c+1)