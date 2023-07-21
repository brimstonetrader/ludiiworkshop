> inst :: Int -> Int
> inst r = ((1664525*r+1013904223) `mod` (2 ^ 32)) 

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

> randPath :: Int -> Int -> IO ()
> randPath seed n = do 
>   let rnds = permute seed [1..16]
>   putStr (convert 0 n rnds)

> data Move where
>   Diag :: Limit  -> Move
>   Orth :: Limit  -> Move
>   Hipp :: Coord  -> Limit -> Move
>   Mult :: [Move] -> Move

> data Limit where
>   Num   :: Int        -> Limit
>   Range :: (Int, Int) -> Limit
>   List  :: [Int]      -> Limit
>   None  ::               Limit

> type Coord = (Int, Int)


> convert :: Int -> Int -> [Int] -> String
> convert i n [] = "\n"
> convert i n (r:rnds) = if i%n==n-1 then ((intToChar r):"\n") ++ (convert (i+1) n rnds) else (intToChar r):(convert (i+1) n rnds)

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
> intToChar x  = intToChar (x%16)















