> [[Int]] :: Matrix

> zeroMatrix :: Int -> Matrix








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

> rep :: Int -> a -> [a]
> rep 0 b = []
> rep a b = b:(rep (a-1) b)



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


> isPrime :: Int -> Bool 
> isPrime p = if p `mod` 2 == 0 then False 
>               else isPrime2 p 3 (sq p) 

> isPrime2 :: Int -> Int -> Int -> Bool 
> isPrime2 p i n = if i>n  then True 
>   else if p `mod` i == 0 then False 
>                          else isPrime2 p (i+2) n

> nextPrime :: Int -> Int  
> nextPrime n = if isPrime n then n else nextPrime (n+1)