-------------------------------------------------------
--
-- The Sort Functions

> sort :: [Integer] -> [Integer]
> sort      [] = []
> sort     [x] = [x]
> sort   [x,y] = case (x>y) of
>      False -> [x,y]
>      True  -> [y,x]  
> sort list    = do
>   let (as, b:bs)  = splitAt ((length(list)) `div` 2) list
>   let [cs, ds]    = pivotAbout b as [[],[]]
>   let [es, fs]    = pivotAbout b bs [[],[]]
>   (sort (cs++es)) ++ (b:(sort (ds++fs)))

> isSorted :: [Integer] -> Bool 
> isSorted [_] = True
> isSorted (x:y:zs) = if x > y then False else isSorted (y:zs)

> pivotAbout :: Integer -> [Integer] -> [[Integer]] -> [[Integer]]
> pivotAbout n []     [a,b]  = [a,b]
> pivotAbout n [x]    [a,b]  = if (x<n) 
>                          then [x:a,b] 
>                          else [a,x:b]
> pivotAbout n (x:xs) [a,b]  = if (x<n) 
>                          then pivotAbout n xs [x:a,b] 
>                          else pivotAbout n xs [a,x:b]

----------------------------------------------------------------
--
-- Random Number Generator

-- Standard

--  Chapter 7.1, Eq. 7.1.6
--  parameters from Knuth and H. W. Lewis

> lcg :: Integer -> Integer -> Integer -> [Integer]
> lcg cap ornd 1  = [(inst ornd)]
> lcg cap ornd it = do 
>   let nrnd = inst ornd 
>   (nrnd : lcg cap nrnd (it-1))

-- Permutes



----------------------------------------------------------------
--
-- Boilerplate


> oneto :: Integer -> [Integer]
> oneto n = oneto2 0 n []

> oneto2 :: Integer -> Integer -> [Integer] -> [Integer]
> oneto2 a b cs = if a==b then cs else oneto2 a (b-1) (b:cs)

> setIn :: Integer -> [Integer] -> Bool
> setIn a (b:bs) = if a==b then True else setIn a bs

> binIn :: Integer -> [Integer] -> Bool
> binIn a []  = False
> binIn a bs = do
>   let cs = if isSorted bs then bs else sort bs
>   let (ds, e:fs)  = splitAt (fromIntegral ((len cs) `div` 2)) cs 
>   if e>a then binIn a ds else if e<a then binIn a fs else True

> list1ton :: Integer -> [Integer]
> list1ton n | n<1 = []
> list1ton n = n:(list1ton (n-1))

> len :: [Integer] -> Integer
> len []     = 0
> len (a:as) = 1+len as

> amanybs :: Integer -> Integer -> [Integer]
> amanybs 0 b = []
> amanybs a b = (b:amanybs (a-1) b)

> rep :: Integer -> [a] -> [a]
> rep a bs = rep2 bs a []

> rep2 :: [a] -> Integer -> [a] -> [a]
> rep2 _  0 bs = bs
> rep2 as i bs = as++(rep2 as (i-1) bs)

----------------------------------------------------------------
--
-- Set Functions

> intersection :: [Integer] -> [Integer] -> [Integer]
> intersection []     bs = []
> intersection (a:as) bs = if a `elem` bs then a:(intersection as bs) else intersection as bs

> union        :: [Integer] -> [Integer] -> [Integer]
> union as [] = sort as
> union as (b:bs) = sort (removeRepeats (as ++ bs))

> difference :: [Integer] -> [Integer] -> [Integer]
> difference as bs = sort (difference2 as (intersection as bs))

> difference2 :: [Integer] -> [Integer] -> [Integer]
> difference2 []     bs = []
> difference2 (a:as) bs = if a `elem` bs then difference2 as bs else a:(difference2 as bs)

> disjunction :: [Integer] -> [Integer] -> [Integer]
> disjunction as bs = difference (union as bs) (intersection as bs)

> removeRepeats :: [Integer] -> [Integer]
> removeRepeats [] = []
> removeRepeats (a:as) = if a `elem` as then removeRepeats as else a : removeRepeats as

> lcg2 :: Integer -> Integer -> [Integer]
> lcg2 _ 1  = [1]
> lcg2 r i = do 
>   let n = inst r 
>   (((n `mod` i)+1) : (lcg2 n (i-1)))

> permute :: Integer -> [Integer] -> [Integer]
> permute seed as = do 
>   let bs = lcg2 seed (len as)
>   permute2 as bs []

> permute2 :: [Integer] -> [Integer] -> [Integer] -> [Integer]
> permute2 as []     cs = cs 
> permute2 as (b:bs) cs = do
>   let n          = len as
>   let (ds, e:es) = splitAt (fromIntegral (b-1)) as
>   permute2 (ds++es) bs (e:cs) 


> square :: [Integer] -> [(Integer, Integer)]
> square as = do 
>   let l = len as
>   square2 as (rep l as) l ((l*l)-1) []

> square2 :: [Integer] -> [Integer] -> Integer -> Integer -> [(Integer, Integer)] -> [(Integer, Integer)]
> square2 [a]    (b:bs)    _ _ cs = (a,b):cs
> square2 (a:as) [b]    _ _ cs = (a,b):cs
> square2 (a:as) (b:bs) l i cs = if i `mod` l == 0 then square2    as  bs l (i-1) ((a,b):cs)
>                                                  else square2 (a:as) bs l (i-1) ((a,b):cs)

> max :: [Integer] -> Integer
> max []       = 0
> max [a]      = a
> max (a:b:cs) = if a>b then max (a:cs) else max (b:cs)

----------------------------------------------------------------
--
-- Meme Functions

> choose1 :: Integer -> Integer -> Integer
> choose1 n k = if n==k || n<1 then 1 else (choose1 (n-1) (k-1))+(choose1 (n-1) k)

> choose2 :: Integer -> Integer -> [Integer]
> choose2 n k = if n==k then [1] else (choose (n-1) (k-1)) : (choose2 (n-1) k)

> choose :: Integer -> Integer -> Integer
> choose n k = (fall n k) `div` (factorial (n-k))

> factorial :: Integer -> Integer
> factorial 1 = 1
> factorial n = n*(factorial (n-1))




> fall :: Integer -> Integer -> Integer
> fall n k = if n==k then 1 else n*(fall (n-1) k)

> ack :: Integer -> Integer -> Integer
> ack 0 n = n+1
> ack m 0 = ack (m-1) 1
> ack m n = ack (m-1) (ack m (n-1))

> sq :: Integer -> Integer
> sq n = sqrt2 0 n

> sqrt2 :: Integer -> Integer -> Integer
> sqrt2 x y = if (x*x)>y then x-1 else sqrt2 (x+1) y

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

> inst :: Integer -> Integer
> inst r = ((1664525*r+1013904223) `mod` (2 ^ 32)) 

> rng :: Integer -> Integer -> [Integer]
> rng ornd 1  = [(inst ornd)]
> rng ornd it = do 
>   let nrnd = inst ornd 
>   (nrnd : rng nrnd (it-1))

-- The LCG has disappointingly small modular periods, but
-- an integer division of 99 seems to clean it up.

> listMod :: [Integer] -> Integer -> [Integer]
> listMod [] _ = []
> listMod (a:as) b = ((a `div` 99) `mod` 2):(listMod as b)

-- This function reflects a square list 45 degrees, like

-- 111    100
-- 001 -> 100
-- 001    111

-- turns rows to columns, and vice versa.

> refl :: Integer -> Integer -> [Integer] -> [Integer]
> refl i n os = if i==(n*n) then [] else 
>   (os !! (fromIntegral ((n*(i `mod` n)) + (i `div` n)))):(refl (i+1) n os)

-- Determines total count of a row.

> rows :: Integer -> Integer -> Integer -> [Integer] -> [Integer] -> [Integer]
> rows i n r rs []     = r:rs
> rows i n r rs (q:qs) = if i `mod` n == 0 
>                      then rows (i+1) n 0     (q+r:rs) qs 
>                      else rows (i+1) n (q+r)    rs  qs


-- Determines count of both rows and columns of a square list. Outputs
-- ((rows, columns), square) like
-- 
--   2 2 2 2
-- 3 X X O X
-- 2 X O X O -> (([3,2,3,1],[2,2,2,2]),[1,1,0,1,1,0,1,0,0,1,1,1,0,1,0,0])
-- 3 O X X X
-- 1 O X O O

> nonogram :: Integer -> Integer -> (([Integer], [Integer]), [Integer])
> nonogram seed n = do
>   let rnds = (listMod (rng seed (n*n)) 2)
>   let r:rs = rows 1 n 0 [] rnds
>   let c:cs = rows 1 n 0 [] (refl 0 n rnds)
>   ((reverse cs,reverse rs),rnds)

-- Prints a blank grid of size n.

> printGrid :: Integer -> IO ()
> printGrid n = do 
>   putStr ((rep n " _") ++ "\n")
>   printGrid2 n n

> printGrid2 :: Integer -> Integer -> IO ()
> printGrid2 n 1 = putStr (('|':(rep n "_|")) ++ "\n")
> printGrid2 n i = do 
>   putStr (('|':(rep n "_|")) ++ "\n")
>   printGrid2 n (i-1)

-- Handles spacing minutae

> toStr :: [Integer] -> String
> toStr []     = ""
> toStr (a:as) = (if a < 10 then (show a) ++ " " else show a) ++ toStr as 

-- Prints hints on blank grid.

> unsPuzzle :: Integer -> Integer -> IO ()
> unsPuzzle seed n = do
>   let ((as,bs),cs) = nonogram seed n
>   let s1 = (toStr as)
>   putStr (' ':s1)
>   printGridU bs n n

> printGridU :: [Integer] -> Integer -> Integer -> IO ()
> printGridU (m:ms) n 1 = putStr ((show m) ++ (('|':(rep n "_|")) ++ "\n"))
> printGridU (m:ms) n i = do 
>   putStr ((show m) ++ (('|':(rep n "_|")) ++ "\n"))
>   printGridU ms n (i-1)

-- Prints hints onto solved grid.

> slvPuzzle :: Integer -> Integer -> IO ()
> slvPuzzle seed n = do
>   let ((as,bs),cs) = nonogram seed n
>   let s1 = ' ':(toStr as)
>   putStr (' ':s1)
>   printGridS bs cs n (n*n)

> printGridS :: [Integer] -> [Integer] -> Integer -> Integer -> IO ()
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


-- To make these nonograms real, we need not total count, but counts of groups.
-- So [1,1,0,0,1,0,1,1,1,0] -> [2,1,3]

> runsOfOnes :: [Integer] -> [Integer]
> runsOfOnes as = reverse (runsOfOnes2 0 as [])

> runsOfOnes2 :: Integer -> [Integer] -> [Integer] -> [Integer]
> runsOfOnes2 i [] bs = if i>0 then i:bs else bs
> runsOfOnes2 i (1:as) bs = runsOfOnes2 (i+1) as bs
> runsOfOnes2 i (a:as) bs = if i>0 then runsOfOnes2 0 as (i:bs) 
>                                  else runsOfOnes2 0 as bs 

-- Helpers

> extract :: Integer -> [Integer] -> [Integer] 
> extract 0 as     = []
> extract n (a:as) = a:(extract (n-1) as)

> retract :: Integer -> [Integer] -> [Integer] 
> retract 0 as     = as
> retract n (a:as) = retract (n-1) as

> applyROO :: Integer -> [Integer] -> [[Integer]]
> applyROO n [] = [[]]
> applyROO n ns = (runsOfOnes (extract n ns)):(applyROO n (retract n ns))

> nonoGram :: Integer -> Integer -> (([[Integer]], [[Integer]]), [Integer])
> nonoGram seed n = do
>   let rnds = (listMod (rng seed (n*n)) 2)
>   let rs = init (applyROO n rnds)
>   let cs = init (applyROO n (refl 0 n rnds))
>   ((cs,rs),rnds)

-- Prints hints on blank grid.

> unsPuzzle2 :: Integer -> Integer -> IO ()
> unsPuzzle2 seed n = do
>   let ((as,bs),cs) = nonogram seed n
>   let s1 = (toStr as)
>   putStr (' ':s1)
>   printGridU2 bs n n

> printGridU2 :: [Integer] -> Integer -> Integer -> IO ()
> printGridU2 (m:ms) n 1 = putStr ((show m) ++ (('|':(rep n "_|")) ++ "\n"))
> printGridU2 (m:ms) n i = do 
>   putStr ((show m) ++ (('|':(rep n "_|")) ++ "\n"))
>   printGridU2 ms n (i-1)

-- Prints hints onto solved grid.

> slvPuzzle2 :: Integer -> Integer -> IO ()
> slvPuzzle2 seed n = do
>   let ((as,bs),cs) = nonogram seed n
>   let s1 = ' ':(toStr as)
>   putStr (' ':s1)
>   printGridS2 bs cs n (n*n)

> printGridS2 :: [Integer] -> [Integer] -> Integer -> Integer -> IO ()
> printGridS2 [] [m]    _ _ = putStr (if m == 1 then "X|" else "_|")
> printGridS2 [] (m:ms) n i = do 
>   putStr (if m == 1 then "X|" else "_|")
>   printGridS2 [] ms n (i-1)
> printGridS2 (l:ls) (m:ms) n i = do 
>   let s = if m == 1 then "X|" else "_|"
>   let t = if i `mod` n == 0 
>           then ("\n" ++ (if l < 10 
>                          then (show l) ++ " " 
>                          else show l) ++ "|") 
>           else ""
>   putStr t
>   putStr s
>   printGridS2 (if t=="" then (l:ls) else ls) ms n (i-1) 





















> randgrid :: Integer -> Integer -> [[Integer]]
> randgrid seed n = randgrid2 (lcg 100000 seed n) n []
   
> randgrid2 :: [Integer] -> Integer -> [[Integer]] -> [[Integer]]
> randgrid2 [r] n g      =                   (permute r (oneto n)):g
> randgrid2 (r:rnds) n g = randgrid2 rnds n ((permute r (oneto n)):g)























-- That's not great.

> initgrid :: Integer -> Integer -> [[Integer]]
> initgrid seed n = do 
>   let [a,b,c] = (lcg 100000 seed 3) 
>   let ds      = permute a (oneto (n*n)) 
>   let es      = permute b (oneto (n*n))
>   let fs      = permute c (oneto (n*n))
>   let gs      = difference es (extract n ds)
>   arrange (n*n) (n*n) ds gs fs []

> arrange :: Integer -> Integer -> [Integer] -> [Integer] -> [Integer] -> [[Integer]]  -> [[Integer]] 
> arrange _ 0 _  _  _      g = reverse g
> arrange n i (h:hs) bs vs g = arrange n (i-1) [] bs (difference vs [h]) [(h:hs)]
> arrange n i [] (b:bs) vs g = arrange n (i-1) [] (retract (sq n) (b:bs)) (difference vs [b])
> 												  (((extract (sq n) (b:bs)) ++ (amanybs (n - (sq n)) 0)):g)
> arrange n i [] [] (v:vs) g = arrange n (i-1) [] [] vs ((v:(amanybs (n-1) 0)):g)   
				
> initfill :: Integer -> Integer -> [Integer] -> [[Integer]] -> [[Integer]] -> [[Integer]] 
> initfill n i (r:rnds) hnts cnvs = if i > n*n 
>   then cnvs 
>	else do 
>     let m  = i `mod` n
>     let d  = i `div` n
>     if (cnvs !! (fromIntegral d) !! (fromIntegral m)) > 0 
>          then initfill n (i+1) (r:rnds) hnts cnvs 
>          else do
>            let ps = (findOptions 0 n m d cnvs [])
>            [[0]]



> findOptions :: Integer -> Integer -> Integer -> Integer -> [[Integer]] -> [Integer] -> [Integer]
> findOptions _ _ _ _ []       nums = (difference (oneto 9) nums)
> findOptions i n m d ([]:cnvs) nums = findOptions (i+1) n m d cnvs nums
> findOptions i n m d ((c:cs):cnvs) nums = if i `div` n < d 
>   then findOptions (i+1) n m d (cs:cnvs) (c:nums)
>   else if i `mod` n < m 
>   then findOptions (i+1) n m d (cs:cnvs) (c:nums)
>   else if i `div` (sq n) < 1 && (i `div` n) `div` (sq n) < 1 
>   then findOptions (i+1) n m d (cs:cnvs) (c:nums)
>   else findOptions (i+1) n m d (cs:cnvs) nums







----------------------------------------------------------------
















----------------------------------------------------------------
----------------------------------------------------------------
----------------------------------------------------------------
----------------------------------------------------------------
----------------------------------------------------------------
----------------------------------------------------------------
----------------------------------------------------------------




rng :: Integer -> Integer -> [Integer]
rng ornd 1  = [(inst ornd)]
rng ornd it = do 
  let nrnd = inst ornd 
  (nrnd : rng nrnd (it-1))


 -- _ _ _ _ _ _ _ _ _ _ _ 
-- |_|_|_|_|_|_|_|_|_|_|_| 














