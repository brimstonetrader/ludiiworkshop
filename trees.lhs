
----------------------------------------------------------------
--
-- Tents
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
>   let n          = len as
>   let (ds, e:es) = splitAt (fromIntegral (b-1)) as
>   permute2 (ds++es) bs (e:cs) 

> noiseGrid :: Int -> Int -> Int -> [Int]
> noiseGrid a b seed = permute seed ((amanybs a 1) ++ (amanybs (b-a) 0))



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

> nonogram :: Int -> Int -> Int -> (([Int], [Int]), [Int])
> nonogram seed a n = do
>   let rnds = noiseGrid a (n*n) seed 
>   let c:cs = ed 1 n 0 [] rnds
>   let r:rs = ed 1 n 0 [] (refl 0 n rnds)
>   ((reverse rs,reverse cs),rnds)

> printGrid :: Int -> IO ()
> printGrid n = do 
>   putStr ((rep n " _") ++ "\n")
>   printGrid2 n n

> printGrid2 :: Int -> Int -> IO ()
> printGrid2 n 1 = putStr (('|':(rep n "_|")) ++ "\n")
> printGrid2 n i = do 
>   putStr (('|':(rep n "_|")) ++ "\n")
>   printGrid2 n (i-1)


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


> toStr :: [Int] -> String
> toStr []     = ""
> toStr (a:as) = (if a < 10 then ' ':(show a) else show a) ++ toStr as 

> len :: [a] -> Int
> len []     = 0
> len (a:as) = 1+(len as)

> rep :: Int -> [a] -> [a]
> rep a bs = rep2 bs a []

> rep2 :: [a] -> Int -> [a] -> [a]
> rep2 _  0 bs = bs
> rep2 as i bs = as++(rep2 as (i-1) bs)


> amanybs :: Int -> a -> [a]
> amanybs 0 b = []
> amanybs a b = (b:amanybs (a-1) b)