
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
-- an Int division of 99 seems to clean it up.

> listMod :: [Int] -> Int -> [Int]
> listMod [] _ = []
> listMod (a:as) b = ((a `div` 99) `mod` 2):(listMod as b)



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


-- Reflects a square list 45 degrees, like

   111    100
   001 -> 100
   001    111

-- turning rows to columns, and vice versa.

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




-- Determines count of both rows and columns of a square list. Outputs
-- ((rows, columns), square) like
-- 
	 2 2 2 2
   3 X X O X
   2 X O X O -> (([3,2,3,1],[2,2,2,2]),[1,1,0,1,1,0,1,0,0,1,1,1,0,1,0,0])
   3 O X X X
   1 O X O O

> ed :: Int -> Int -> Int -> [Int] -> [Int] -> [Int]
> ed i n r rs []     = r:rs
> ed i n r rs (q:qs) = if i `mod` n == 0 
>                      then ed (i+1) n 0     (q+r:rs) qs 
>                      else ed (i+1) n (q+r)    rs  qs

> nonogram :: Int -> Int -> (([Int], [Int]), [Int])
> nonogram seed n = do
>   let rnds = (listMod (rng seed (n*n)) 2)
>   let c:cs = ed 1 n 0 [] rnds
>   let r:rs = ed 1 n 0 [] (refl 0 n rnds)
>   ((reverse rs,reverse cs),rnds)

-- ghci> nonogram 687248978 6
   (([3,4,4,4,3,3],
	 [4,6,1,2,3,5]),
  
	 [0,1,1,1,1,0,
	  1,1,1,1,1,1,
	  0,1,0,0,0,0,
	  1,0,0,1,0,0,
	  0,0,1,1,0,1,
	  1,1,1,0,1,1])

-- Prints a blank grid of size n.

> printGrid :: Int -> IO ()
> printGrid n = do 
>   putStr ((rep n " _") ++ "\n")
>   printGrid2 n n

> printGrid2 :: Int -> Int -> IO ()
> printGrid2 n 1 = putStr (('|':(rep n "_|")) ++ "\n")
> printGrid2 n i = do 
>   putStr (('|':(rep n "_|")) ++ "\n")
>   printGrid2 n (i-1)


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

-- Prints hints on blank grid.

> unsPuzzle :: Int -> Int -> IO ()
> unsPuzzle seed n = do
>   let ((as,bs),cs) = nonogram seed n
>   let s1 = (toStr as) ++ "\n"
>   putStr (' ':s1)
>   printGridU bs n n

> printGridU :: [Int] -> Int -> Int -> IO ()
> printGridU (m:ms) n 1 = putStr ((show m) ++ (('|':(rep n "_|")) ++ "\n"))
> printGridU (m:ms) n i = do 
>   putStr ((show m) ++ (('|':(rep n "_|")) ++ "\n"))
>   printGridU ms n (i-1)

-- Prints hints onto solved grid.

> slvPuzzle :: Int -> Int -> IO ()
> slvPuzzle seed n = do
>   let ((as,bs),cs) = nonogram seed n
>   let s1 = ' ':(toStr as)
>   putStr (' ':s1)
>   printGridS bs cs n (n*n)

> printGridS :: [Int] -> [Int] -> Int -> Int -> IO ()
> printGridS [] [m]    _ _ = putStr (if m == 1 then "X|" else "_|")
> printGridS [] (m:ms) n i = do 
>   putStr (if m == 1 then "X|" else "_|")
>   printGridS [] ms n (i-1)
> printGridS (l:ls) (m:ms) n i = do 
>   let s = if m == 1 then "X|" else "_|"
>   let t = if i `mod` n == 0 
>           then ("\n" ++ (if l < 10 
>                          then (show l) ++ " " 
>                          else show l) ++ "|") 
>           else ""
>   putStr t
>   putStr s
>   printGridS (if t=="" then (l:ls) else ls) ms n (i-1) 

-- ghci> unsPuzzle 87632786 10

  8 4 6 3 5 5 5 6 6 3
7|_|_|_|_|_|_|_|_|_|_|
6|_|_|_|_|_|_|_|_|_|_|
3|_|_|_|_|_|_|_|_|_|_|
4|_|_|_|_|_|_|_|_|_|_|
5|_|_|_|_|_|_|_|_|_|_|
3|_|_|_|_|_|_|_|_|_|_|
4|_|_|_|_|_|_|_|_|_|_|
7|_|_|_|_|_|_|_|_|_|_|
6|_|_|_|_|_|_|_|_|_|_|
6|_|_|_|_|_|_|_|_|_|_|

-- ghci> slvPuzzle 87632786 10

   8 4 6 3 5 5 5 6 6 3
7 |X|X|X|_|X|_|_|X|X|X|
6 |X|_|_|X|X|X|X|_|X|_|
3 |_|_|_|X|X|_|X|_|_|_|
4 |X|_|X|_|_|_|X|X|_|_|
5 |X|X|_|_|_|_|_|X|X|X|
3 |_|X|_|_|_|X|_|_|X|_|
4 |X|_|X|_|_|X|X|_|_|_|
7 |X|X|X|_|X|X|_|X|X|_|
6 |X|_|X|_|X|X|_|X|X|_|
6 |X|_|X|X|_|_|X|X|_|X|














-------------------------------------------------------------------------------
-- To make these nonograms real, we need not total count, but counts of groups.
-- So [1,1,0,0,1,0,1,1,1,0] -> [2,1,3] nstad 

> runsOfOnes :: [Int] -> [Int]
> runsOfOnes as = reverse (runsOfOnes2 0 as [])

> runsOfOnes2 :: Int -> [Int] -> [Int] -> [Int]
> runsOfOnes2 i [] bs = if i>0 then i:bs else bs
> runsOfOnes2 i (1:as) bs = runsOfOnes2 (i+1) as bs
> runsOfOnes2 i (a:as) bs = if i>0 then runsOfOnes2 0 as (i:bs) 
>                                  else runsOfOnes2 0 as bs 

> applyROO :: Int -> [Int] -> [[Int]]
> applyROO n [] = [[]]
> applyROO n ns = (runsOfOnes (extract n ns)):(applyROO n (retract n ns))

> nonoGram :: Int -> Int -> (([[Int]], [[Int]]), [Int])
> nonoGram seed n = do
>   let rnds = (listMod (rng seed (n*n)) 2)
>   let rs = init (applyROO n rnds)
>   let cs = init (applyROO n (refl 0 n rnds))
>   ((cs,rs),rnds)

-- ghci> nonoGram 87328678 4

	(([[  3],[  1],[1],[2]],
	  [[1,2],[2,1],[1],[ ]]
	   
	   ),[1,0,1,1,
		  1,1,0,1,
		  1,0,0,0,
		  0,0,0,0])



-- Adds zeroes to front of sublists as needed, making each the same length

> pad :: [[Int]] -> [[Int]]
> pad xss = do
>   let m = maxlength xss 
>   reverse (pad2 m xss [])

> pad2 :: Int -> [[Int]] -> [[Int]] -> [[Int]]
> pad2 m [] nss = nss
> pad2 m (xs:xss) nss = pad2 m xss ((((amanybs (m - (len xs)) 0) ++ xs)):nss)

-- ghci> pad [[4,1,1,1],[2,2],[3],[]]

		[[4,1,1,1],
		 [0,0,2,2],
		 [0,0,0,3],
		 [0,0,0,0]]
					 
					 
-- Reorients a list of lists, like refl did earlier.

> refactor :: [[Int]] -> [[Int]]
> refactor [[]] = [[]] 
> refactor nss  = if sumll nss == 0 
>   then [[]] else do
>   let (as, bs) = refactor2 [] [[]] nss 
>   (reverse as):(refactor (reverse bs))

> refactor2 :: [Int] -> [[Int]] -> [[Int]] -> ([Int], [[Int]])
> refactor2 as bss [[]]          = (as,bss)
> refactor2 as bss ((c:[]):[])   = ((c:as),bss)
> refactor2 as bss ((c:cs):[])   = ((c:as),(cs:bss))
> refactor2 as bss ([]:css)      = refactor2 as bss css
> refactor2 as bss ((c:[]):css)  = refactor2 (c:as) ([]:bss) css
> refactor2 as bss ((c:cs):css)  = refactor2 (c:as) (cs:bss) css

-- ghci> refactor [[4,1,1,1],[0,0,2,2],[0,0,0,3],[0,0,0,0]]

		[[4,0,0,0],
		 [1,0,0,0],
		 [1,2,0,0],
		 [1,2,3,0],[]]




-- The main functions.

> slvPuzzle2 :: Int -> Int -> IO ()
> slvPuzzle2 seed n = do
>   let ((css,rss),grid) = nonoGram seed n
>   let yo = (refactor (pad css))
>   putStr (toStr2 (maxlength rss) n ([]:yo))
>   putStr (printGridS2 (pad rss) (maxlength rss) n grid)				
				
> printGridS2 :: [[Int]] -> Int -> Int -> [Int] -> String
> printGridS2 [] _ _ []           = ""
> printGridS2 [] m n (g:grid)     = (if g == 1 then 'X' else '_'):'|':(printGridS2 [] m n grid)
> printGridS2 (l:ls) m n (g:grid) = do 
>   let v = len (g:grid)
>   let f = if v == n*n then "" else "\n"
>   let u = if g == 1 then 'X' else '_'
>   if v `mod` n == 0
>		then (f ++ (toStr3 l) ++ ('|':u:'|':(printGridS2 ls m n grid)))
>	    else (u:'|':(printGridS2 (l:ls) m n grid))


> unsPuzzle2 :: Int -> Int -> IO ()
> unsPuzzle2 seed n = do
>   let ((css,rss),grid) = nonoGram seed n
>   let yo = (refactor (pad css))
>   putStr (toStr2 (maxlength rss) n ([]:yo))
>   putStr (printGridU2 (pad rss) (maxlength rss) n grid)				
				
> printGridU2 :: [[Int]] -> Int -> Int -> [Int] -> String
> printGridU2 [] _ _ []           = ""
> printGridU2 [] m n (g:grid)     = '_':'|':(printGridU2 [] m n grid)
> printGridU2 (l:ls) m n (g:grid) = do 
>   let v = len (g:grid)
>   let f = if v == n*n then "" else "\n"
>   let u = '_'
>   if v `mod` n == 0
>		then (f ++ (toStr3 l) ++ ('|':u:'|':(printGridU2 ls m n grid)))
>	    else (u:'|':(printGridU2 (l:ls) m n grid))



-- ghci> slvPuzzle2 7239778789 15

									   1
		   3   1                     1 2
		   1 4 3   3   2 1   1 2 4 1 1 1
		   1 1 3 1 3 6 2 2   2 1 2 3 2 2
		   1 1 1 3 1 2 2 3 9 2 2 1 1 1 1
		   1 1 2 2 1 2 2 2 3 1 1 4 1 1 1
 3 1 1 3 1|_|_|X|X|X|_|X|_|X|_|X|X|X|_|X|
 1 3 1 2 1|X|_|_|_|X|X|X|_|X|_|X|X|_|X|_|
   3 2 2 1|X|X|X|_|X|X|_|_|X|X|_|X|_|_|_|
   3 4 1 2|X|X|X|_|_|X|X|X|X|_|_|X|_|X|X|
   2 3 2 1|_|X|X|_|X|X|X|_|X|X|_|_|_|_|X|
   2 2 3 2|X|X|_|_|X|X|_|X|X|X|_|_|X|X|_|
	 2 2 4|_|_|_|_|X|X|_|X|X|_|X|X|X|X|_|
   3 2 2 1|X|X|X|_|_|_|_|_|X|X|_|X|X|_|X|
		 9|_|_|X|X|X|X|X|X|X|X|X|_|_|_|_|
   3 3 2 1|_|X|X|X|_|X|X|X|_|_|X|X|_|_|X|
	 1 1 3|_|_|_|X|_|_|_|X|_|_|_|_|X|X|X|
   1 1 2 1|X|_|X|_|_|_|_|_|X|X|_|X|_|_|_|
 1 2 1 2 1|_|_|_|X|_|X|X|_|X|_|X|X|_|_|X|
	 4 4 1|X|X|X|X|_|X|X|X|X|_|_|X|_|_|_|
   1 1 1 4|_|_|X|_|X|_|_|X|_|_|_|X|X|X|X|
   
   
-- ghci> unsPuzzle2 7239778789 15

									   1
		   3   1                     1 2
		   1 4 3   3   2 1   1 2 4 1 1 1
		   1 1 3 1 3 6 2 2   2 1 2 3 2 2
		   1 1 1 3 1 2 2 3 9 2 2 1 1 1 1
		   1 1 2 2 1 2 2 2 3 1 1 4 1 1 1
 3 1 1 3 1|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
 1 3 1 2 1|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
   3 2 2 1|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
   3 4 1 2|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
   2 3 2 1|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
   2 2 3 2|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
	 2 2 4|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
   3 2 2 1|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
		 9|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
   3 3 2 1|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
	 1 1 3|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
   1 1 2 1|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
 1 2 1 2 1|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
	 4 4 1|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|
   1 1 1 4|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|


> make :: Int -> Int -> Int -> IO ()
> make 0 n seed = putStr ""
> make m n seed = do
>   unsPuzzle2 seed n
>   putStr "\n"
>   slvPuzzle2 seed n
>   putStr "\n"
>   make (m-1) n (seed*seed)



-- ghci> make 3 10 9898989

			   2
		 2   2 2 2 3 1   1
		 1 1 2 1 1 1 1 1 3 1
		 2 2 1 1 3 1 1 1 1 1
	 1 6|_|_|_|_|_|_|_|_|_|_|
	   3|_|_|_|_|_|_|_|_|_|_|
   1 2 2|_|_|_|_|_|_|_|_|_|_|
   1 2 1|_|_|_|_|_|_|_|_|_|_|
   1 2 2|_|_|_|_|_|_|_|_|_|_|
	   1|_|_|_|_|_|_|_|_|_|_|
   1 4 1|_|_|_|_|_|_|_|_|_|_|
   1 1 1|_|_|_|_|_|_|_|_|_|_|
	 3 1|_|_|_|_|_|_|_|_|_|_|
   1 1 2|_|_|_|_|_|_|_|_|_|_|
  
			   2
		 2   2 2 2 3 1   1
		 1 1 2 1 1 1 1 1 3 1
		 2 2 1 1 3 1 1 1 1 1
	 1 6|_|X|_|X|X|X|X|X|X|_|
	   3|_|_|_|X|X|X|_|_|_|_|
   1 2 2|_|_|X|_|_|X|X|_|X|X|
   1 2 1|X|_|X|X|_|_|_|_|X|_|
   1 2 2|X|_|_|X|X|_|_|X|X|_|
	   1|_|_|X|_|_|_|_|_|_|_|
   1 4 1|X|_|X|X|X|X|_|_|X|_|
   1 1 1|_|X|_|_|X|_|_|_|_|X|
	 3 1|X|X|X|_|X|_|_|_|_|_|
   1 1 2|X|_|_|X|_|X|X|_|_|_|

		 1
		 1   1 1     2   5
		 1 2 2 1 1 1 1 2 1 2
		 1 2 2 1 3 1 1 1 1 4
   2 1 1|_|_|_|_|_|_|_|_|_|_|
   2 1 4|_|_|_|_|_|_|_|_|_|_|
	   5|_|_|_|_|_|_|_|_|_|_|
	 1 1|_|_|_|_|_|_|_|_|_|_|
	 2 2|_|_|_|_|_|_|_|_|_|_|
 1 1 1 1|_|_|_|_|_|_|_|_|_|_|
	 2 1|_|_|_|_|_|_|_|_|_|_|
 1 2 1 2|_|_|_|_|_|_|_|_|_|_|
 1 1 1 1|_|_|_|_|_|_|_|_|_|_|
   1 1 1|_|_|_|_|_|_|_|_|_|_|

		 1
		 1   1 1     2   5
		 1 2 2 1 1 1 1 2 1 2
		 1 2 2 1 3 1 1 1 1 4
   2 1 1|_|X|X|_|_|X|_|_|X|_|
   2 1 4|X|X|_|X|_|_|X|X|X|X|
	   5|_|_|_|_|_|X|X|X|X|X|
	 1 1|X|_|_|_|_|_|_|_|X|_|
	 2 2|_|_|_|X|X|_|_|X|X|_|
 1 1 1 1|X|_|X|_|_|_|X|_|_|X|
	 2 1|_|X|X|_|_|_|_|_|_|X|
 1 2 1 2|_|X|_|X|X|_|X|_|X|X|
 1 1 1 1|X|_|X|_|X|_|_|_|_|X|
   1 1 1|_|_|X|_|X|_|_|_|X|_|

				 1 1     1
		 3 1 1   2 1 1 1 1 4
		 2 2 1 3 1 1 1 2 1 2
		 1 3 2 2 1 1 1 2 1 1
	 2 6|_|_|_|_|_|_|_|_|_|_|
   1 1 1|_|_|_|_|_|_|_|_|_|_|
   5 1 1|_|_|_|_|_|_|_|_|_|_|
 1 2 1 1|_|_|_|_|_|_|_|_|_|_|
   1 2 3|_|_|_|_|_|_|_|_|_|_|
	 1 1|_|_|_|_|_|_|_|_|_|_|
   1 1 1|_|_|_|_|_|_|_|_|_|_|
   1 1 3|_|_|_|_|_|_|_|_|_|_|
	 2 1|_|_|_|_|_|_|_|_|_|_|
 1 1 3 2|_|_|_|_|_|_|_|_|_|_|

				 1 1     1
		 3 1 1   2 1 1 1 1 4
		 2 2 1 3 1 1 1 2 1 2
		 1 3 2 2 1 1 1 2 1 1
	 2 6|X|X|_|X|X|X|X|X|X|_|
   1 1 1|X|_|_|X|_|_|_|_|_|X|
   5 1 1|X|X|X|X|X|_|X|_|_|X|
 1 2 1 1|_|X|_|_|X|X|_|X|_|X|
   1 2 3|X|_|X|X|_|_|_|X|X|X|
	 1 1|X|_|_|X|_|_|_|_|_|_|
   1 1 1|_|X|_|_|X|_|_|_|_|X|
   1 1 3|_|X|_|_|_|X|_|X|X|X|
	 2 1|_|X|X|_|_|_|_|X|_|_|
 1 1 3 2|X|_|X|_|X|X|X|_|X|X|


-- An interesting challenge is to enforce one solution per grid. For example,

_|1 1
1|_|_|
1|_|_|

-- can be solved in two ways, and

_|1 1 1
1|_|_|_|
1|_|_|_|
1|_|_|_|

-- in three, and so on. 




-------------------------------------------------------------------------

-- Not worth explaining

> toStr :: [Int] -> String
> toStr []     = ""
> toStr (a:as) = (if a < 10 then ' ':(show a) else show a) ++ toStr as 

> toStr2 :: Int -> Int -> [[Int]] -> String
> toStr2 _ _ ([]:[[]])      = "\n"
> toStr2 a n ([]:wss)       = "\n" ++ (rep a "  ") ++ toStr2 a n wss
> toStr2 a n ((0:ws):wss)   = (' ':' ':(toStr2 a n (ws:wss)))
> toStr2 a n ((w:ws):wss)   = ' ':(intToChar w):(toStr2 a n (ws:wss))

> intToChar :: Int -> Char
> intToChar 1  = '1'
> intToChar 2  = '2'
> intToChar 3  = '3'
> intToChar 4  = '4'
> intToChar 5  = '5'
> intToChar 6  = '6'
> intToChar 7  = '7'
> intToChar 8  = '8'
> intToChar 9  = '9'
> intToChar 10 = 'X'
> intToChar 11 = 'E'
> intToChar 12 = 'D'
> intToChar 13 = 'B'
> intToChar 14 = 'F'
> intToChar 15 = 'V'
> intToChar x  = intToChar (x`mod`16)

> toStr3 :: [Int] -> String
> toStr3 []     = ""
> toStr3 (a:as) = if a==0 
>					then ' ':' ':toStr3 as 
>					else ' ':(intToChar a):(toStr3 as)


> rep :: Int -> [a] -> [a]
> rep a bs = rep2 bs a []

> rep2 :: [a] -> Int -> [a] -> [a]
> rep2 _  0 bs = bs
> rep2 as i bs = as++(rep2 as (i-1) bs)

> maxlength :: [[a]] -> Int
> maxlength [[]] = 0
> maxlength [as] = len as
> maxlength (as:bs:css) = do 
>   let la = len as
>   let lb = len bs
>   if (la>lb) then maxlength (as:css) else maxlength (bs:css) 

> extract :: Int -> [Int] -> [Int] 
> extract 0 as     = []
> extract n (a:as) = a:(extract (n-1) as)

> retract :: Int -> [Int] -> [Int] 
> retract 0 as     = as
> retract n (a:as) = retract (n-1) as

> amanybs :: Int -> a -> [a]
> amanybs 0 b = []
> amanybs a b = (b:amanybs (a-1) b)

> sumll :: [[Int]] -> Int
> sumll [[]]         = 0
> sumll ([]:qs)      = sumll qs
> sumll ((q:qs):qss) = q + sumll (qs:qss)

> len :: [a] -> Int
> len []     = 0
> len (a:as) = 1+(len as)

> sq :: Int -> Int
> sq n = sqrt2 0 n

> sqrt2 :: Int -> Int -> Int
> sqrt2 x y = if (x*x)>y then x-1 else sqrt2 (x+1) y