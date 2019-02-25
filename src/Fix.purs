module Fix where 

import Data.Eq
import Data.Functor
import Data.Newtype

import Matryoshka.Class.Recursive

newtype Fix f = Fix (f (Fix f))

fold :: forall f a. Functor f => (f a -> a) -> Fix f -> a 
fold f (Fix fa) = f (map (fold f) fa)

derive instance newtypeFix :: Newtype (Fix f) _

instance eqFix :: Eq1 f => Eq (Fix f) 
  where 
  eq (Fix l) (Fix r) = l `eq1` r

instance recursiveFix :: Functor f => Recursive (Fix f) f 
  where 
  project (Fix f) = f