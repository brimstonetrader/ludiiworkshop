> newtype M x = M (M x -> x) 

-- w = \x -> x x
> w :: (M x -> x) -> x
> w f = f (M f)

-- y f = f ( y f ) = f (f ( y f )) = ...
> y :: (a -> a) -> a
> y f = w (\(M x) -> f (w x))

-- qimokao on stack overflow ty