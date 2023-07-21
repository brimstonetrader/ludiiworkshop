> linepart :: Integer -> [[Integer]]
> linepart n = linepart2 0 (2^(n-1))

> linepart2 :: Integer -> Integer -> [[Integer]]
> linepart2 i n = if i==n then 



> methodA :: Integer -> Integer
> methodA n = do 
>   


ghci> primeFactors 1434
[2,3,239]
ghci> primeFactors 1435
[5,7,41]
ghci> primeFactors 1433
[1433]
ghci> primeFactors 1436
[2,2,359]

framer :: Integer -> [[Integer]]
framer n = framer2 1 2^(2*(n-1)) (rep 1 (2*n-1))

framer2 :: Integer -> Integer -> [Integer] -> [Integer]
framer2 i n os = if i>n then os else [os | d <- [1..(n `div` 2)], n `mod` d == 0]
















> len :: [a] -> Integer
> len []     = 0
> len (a:as) = 1+(len as)

> rep :: Integer -> a -> [a]
> rep 0 b = []
> rep a b = b:(rep (a-1) b)

> amanybs :: Integer -> a -> [a]
> amanybs 0 b = []
> amanybs a b = (b:amanybs (a-1) b)

> (%) :: Integer -> Integer -> Integer
> a % b = a `mod` b

> (//) :: Integer -> Integer -> Integer
> a // b = a `div` b

> (/%/) :: Integer -> Integer -> (Integer, Integer)
> a /%/ b = a `divMod` b

> (~) :: Integer -> Integer -> Integer
> a ~ 0 = a
> 0 ~ b = b
> a ~ b = do
>   let (da,ma) = a /%/ 2
>   let (db,mb) = b /%/ 2
>   if ma == mb 
>     then    2*(da ~ db)
>     else 1+(2*(da ~ db))

> sort :: [Integer] -> [Integer]
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

> pivotAbout :: Integer -> [Integer] -> [[Integer]] -> [[Integer]]
> pivotAbout n []     [a,b]  = [a,b]
> pivotAbout n [x]    [a,b]  = if (x<n) 
>                          then [x:a,b] 
>                          else [a,x:b]
> pivotAbout n (x:xs) [a,b]  = if (x<n) 
>                          then pivotAbout n xs [x:a,b] 
>                          else pivotAbout n xs [a,x:b]

> inst :: Integer -> Integer
> inst r = ((1664525*r+1013904223) `mod` (2 ^ 32)) 

> lcg :: Integer -> Integer -> [Integer]
> lcg ornd 1  = [(inst ornd)]
> lcg ornd it = do 
>   let nrnd = inst ornd 
>   (nrnd : lcg nrnd (it-1))

> lcg2 :: Integer -> Integer -> [Integer]
> lcg2 _ 1  = [1]
> lcg2 r i = do 
>   let n = inst r 
>   (((n `mod` i)+1) : (lcg2 n (i-1)))

> primeFactors :: Int -> [Int]
> primeFactors a = factors2 a primesto10000

> factors2 :: Int -> [Int] -> [Int]
> factors2 a []     = []   
> factors2 a (p:ps) = if a<2 then [] 
>   else if isPrime a then [a] 
>   else do 
>     let (da,ma) = a /%/ p
>     if ma == 0 then p:(factors2 da (p:ps))
>                else factors2 a ps