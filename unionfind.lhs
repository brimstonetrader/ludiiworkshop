> import qualified Data.Map.Strict as M
> import qualified Data.Set as S

> type unionFind = M.Map a a 
> type heightMap = M.Map a Int

> new :: [a] -> (unionFind, heightMap)
> new xs = (M.fromList [(x,x) | x <- xs], M.fromList [(x,1) | x <- xs])

> find :: a -> unionFind -> a
> find x map = case map.lookup x of 
>   Just x  -> x
>   Just y  -> find y map
>   Nothing -> x

> union :: a -> a -> (unionFind, heightMap) -> (unionFind, heightMap)
> union a b (uf, hm) = case (hm.lookup a) > (hm.lookup b) of 
>   False -> do 
>     insert b (hm.lookup a) uf
>     adjust (1 +) b hm
>     return (uf, hm)
>   True  -> do 
>     insert a (hm.lookup b) uf
>     adjust (1 +) a hm
>     return (uf, hm)

> connected :: a -> a -> unionFind -> Bool
> connected a b uf = uf.lookup a == uf.lookup b