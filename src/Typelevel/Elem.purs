module Typelevel.Elem where 

import Data.Functor.Coproduct
import Data.Functor.Coproduct.Syntax
import Typelevel.Choose

-- | A type-level function returning whether `f` is an element in the coproduct `g`. 
class Elem (f :: Type -> Type) (g :: Type -> Type) pos | f g -> pos

-- `f` is an element of itself.
instance elemId :: Elem f f (Found Here) 
else

-- `f` is an element of the coproduct if it is an element of one 
-- or both of the summands in the coproduct `l :+: r` and the choice between 
-- the two is unambigous. 
instance elemCoproduct 
  :: ( Elem f l inL       -- search in the left summand 
     , Elem f r inR       -- search in the right summand 
     , Choose inL inR pos -- choose the result
     ) => Elem f (l :+: r) pos 
else 

-- `f` cannot be an element of `g` because `g` is distinct from `f` and not 
-- a type constructor.
instance elemNot :: Elem f g NotFound
