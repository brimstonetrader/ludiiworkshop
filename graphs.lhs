-- a graph is something like this

-- 1   2
-- |\ /|
-- 3-4-5

-- nodes with connections. 

-- we can represent a graph as a triangular jagged list of booleans. The above, like so


--  [1.2]             -> [0]
--  [1.3 2.3]         -> [1,0]
--  [1.4 2.4 3.4]     -> [1,1,1]
--  [1.5 2.5 3.5 4.5] -> [0,1,0,1]

> inst :: Int -> Int
> inst r = ((1664525*r+1013904223) `mod` (2 ^ 32)) 

> rng :: Int -> Int -> [Int]
> rng ornd 1  = [(inst ornd)]
> rng ornd it = do 
>   let nrnd = inst ornd 
>   (nrnd : rng nrnd (it-1))

> len :: [a] -> Int
> len []     = 0
> len (a:as) = 1+(len as)

> listMod :: [Int] -> Int -> [Int]
> listMod [] _ = []
> listMod (a:as) b = ((a `div` 79) `mod` 2):(listMod as b)

> triangularize :: [Int] -> Int -> [[Int]]
> triangularize []   n = [] 
> triangularize rnds n = do 
>   let (as,bs) = splitAt n rnds
>   as:(triangularize bs (n+1))

> sumll :: [[Int]] -> Int
> sumll [[]]         = 0
> sumll ([]:qs)      = sumll qs
> sumll ((q:qs):qss) = q + sumll (qs:qss)

> randGraph :: Int -> Int -> [[Int]]
> randGraph seed n =  do
>   let rnds = listMod (rng seed (((n-1)*(n-2)) `div` 2)) 2
>   triangularize rnds 1

-- we can tally how many vertices there are by the amount of lists plus one,
-- edges by the total amount of Trues.

-- we could also store it more like a dictionary, like

-- 1 :     3,4
-- 2 :       4,5
-- 3 : 1,    4
-- 4 : 1,2,3,  5 . We can get from a to b quite easily, if we've got our pictures like this. One should note, here, that it
-- 5 :   2,  4     is entirely possible in a graph for a vertex to be connected to itself, but this doesn't affect the EC 
-- at all, because it adds a face and an edge in one fell swoop. So we'll ignore that. If our graph was directed, with arrows 
-- pointing one way or the other instead of ambivalent lines, we might need that other half. We'll ignore that too.

> addZero :: [[Int]] -> [[Int]]
> addZero (gs:[])  = [(0:(reverse gs))]
> addZero (gs:gss) = (0:(reverse gs)):(addZero gss)


> toDict :: [[Int]] -> [[Int]]
> toDict [] = []
> toDict gr = do 
>   let v = 1 + len gr
>   let e = sumll gr
>   toDict2 1 v e gr

> toDict2 :: Int -> Int -> Int -> [[Int]] -> [Int]
> toDict2 i v e gra = if i>v*v then [] else do
>   let cv1 = i `div` v
>   let cv2 = i `mod` v
>   if cv1==cv2 then toDict2 (i+1) v e gra
>   else if cv1>cv2 then if (gra !! cv1) !! cv2 == 1 then cv2:(toDict2 (i+1) v e gra)
>                   else if (gra !! cv2) !! cv1 == 1 then if i `mod` v == v-1 then cv2:(toDict2 (i+1) v e gra)  else cv2:(toDict2 (i+1) v e gra) 

-- There's also the incidence matrix, which is like
--     e_1 e_2 e_3 e_4 e_5 e_6 
-- v_1   1   1   0   0   0   0
-- v_2   0   0   1   1   0   0
-- v_3   1   0   0   0   1   0
-- v_4   0   1   1   0   1   1
-- v_5   0   0   0   1   0   1

-- And the edge matrix: 

-- [1,3] 
-- [1,4] 
-- [2,4] 
-- [2,5] 
-- [3,4] 
-- [4,5] 


-- a graph's Euler Characteristic is its faces-edges+vertices. we can tell if a graph is planar (can be drawn on a flat 
-- surface with no crossings) if this equals 2. If it doesn't, it can be drawn on a surface with a genus equal to (2-EC)/2.

-- The above graph has 
--   3 faces (1.3.4, 2.4.5., and the infinite abyss)
-- - 6 edges 
-- + 5 vertices
-------
--   2, which we could have figured out by noting that it doesn't cross itself. How can we determine how many faces there 
-- are of a general graph? we could try to figure out all the cycles. 

-- a cycle is a sequence of nodes, between each of which there exists a vertex, of which only the first and last are equal.
-- the two main cycles in our graph are [1,3,4,1] and [2,4,5,2]. We could try to find all of them, then try to get the 
-- minimal amount that are connected to each other and span all vertices.


  1
 234
56789