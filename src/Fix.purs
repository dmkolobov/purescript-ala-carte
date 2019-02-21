module Fix where 

import Data.Functor

newtype Fix f = Fix (f (Fix f))

fold :: forall f a. Functor f => (f a -> a) -> Fix f -> a 
fold f (Fix fa) = f (map (fold f) fa)

