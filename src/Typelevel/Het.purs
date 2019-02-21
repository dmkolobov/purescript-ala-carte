module Typelevel.Het where 

import Data.Functor.Coproduct.Syntax

import Type.Prelude 
import Type.Data.Boolean 


-- "worklists", or type-level lists of type constructors
data Nil
data Cons (a :: Type -> Type) b

-- | Collects all summands but one into a worklist 
class Dupl (f :: Type -> Type) (l :: Type) (dup :: Boolean) | f l -> dup 

-- | Recursive case for the worklist. 
class Sift (l :: Type) (dup :: Boolean) | l -> dup

-- | Traverses the elements of the worklist, comparing them with `f`
class Find (f :: Type -> Type) (l :: Type) (dup :: Boolean) | f l -> dup 

-- | Defines the occurances of the summand 'f' in 'g'. 
class IsSummand (f :: Type -> Type) (g :: Type -> Type) (dup :: Boolean) | f g -> dup 

instance dupl1 :: Dupl f (Cons g l) dup => Dupl (f :+: g) l dup else
instance dupl2 :: ( Find f l b , Sift l bs, Or b bs b' ) => Dupl f l b' 

instance sift1 :: ( Dupl f fs b , Sift fs bs, Or b bs b' ) => Sift (Cons f fs) b' else
instance sift2 :: Sift Nil False 

instance find1 :: ( IsSummand f g b , Find f gs bs, Or b bs b' ) => Find f (Cons g gs) b' 
instance find2 :: Find f Nil False 

instance sum1 :: ( IsSummand f g1 b1 , IsSummand f g2 b2, Or b1 b2 b) => IsSummand f (g1 :+: g2) b else 
instance sum2 ::                                              IsSummand f f           True         else 
instance sum3 ::                                              IsSummand f g           False
