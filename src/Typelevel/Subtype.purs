module Typelevel.Subtype where 

import Control.Category

import Data.Either (Either(..))
import Data.Functor.Coproduct
import Data.Functor.Coproduct.Syntax

import Typelevel.Elem 
import Typelevel.Choose
import Typelevel.Het

import Data.Maybe

import Type.Prelude
import Type.Data.Boolean 

class Elem f g res <= Subsume res f g where 
  inj' :: forall a. Proxy res -> f a -> g a 
  prj' :: forall a. Proxy res -> g a -> Maybe (f a) 

instance subsumeHere :: Subsume (Found Here) f f 
  where 
  inj' _ = identity
  prj' _ = Just

instance subsumeL :: ( Elem f (l :+: r) (Found (L res))
                     , Subsume (Found res) f l
                     )
                  => Subsume (Found (L res)) f (l :+: r) 
  where  
  inj' _ fa = left (inj' (Proxy :: Proxy (Found res)) fa)

  prj' _ (Coproduct (Left fa)) = prj' (Proxy :: Proxy (Found res)) fa 
  prj' _ _        = Nothing 

instance subsumeR :: ( Elem f (l :+: r) (Found (R res))
                     , Subsume (Found res) f r
                     )
                  => Subsume (Found (R res)) f (l :+: r) 
  where  
  inj' _ fa = right (inj' (Proxy :: Proxy (Found res)) fa)

  prj' _ (Coproduct (Right fa)) = prj' (Proxy :: Proxy (Found res)) fa 
  prj' _ _        = Nothing 

inj :: forall res f g a. Elem f g res => Subsume res f g => f a -> g a 
inj fa = inj' (Proxy :: Proxy res) fa

prj :: forall res f g a. Elem f g res => Subsume res f g => g a -> Maybe (f a) 
prj ga = prj' (Proxy :: Proxy res) ga

-- "strucs", or a type-level measure of the atomicity of a coproduct 
data Atom res
data Sum l r

-- | Decomposition of a summand on the left ------------------------
--------------------------------------------------------------------

class GetStruc (f :: Type -> Type) (g :: Type -> Type) s | f g -> s

instance coproductStruc :: ( GetStruc f1 g l
                           , GetStruc f2 g r
                           ) 
                           => GetStruc (f1 :+: f2) g (Sum l r) else

instance atomicStruc :: Elem f g res => GetStruc f g (Atom res)

-- | Subsumption with complex arguments on the left ---------------
-------------------------------------------------------------------

class Subtype s f g 
  where 
  inj'' :: forall a. Proxy s -> f a -> g a
  prj'' :: forall a. Proxy s -> g a -> Maybe (f a)

instance subty1 :: Subsume res f g => Subtype (Atom res) f g 
  where 
  inj'' _ = inj' (Proxy :: Proxy res) 
  prj'' _ = prj' (Proxy :: Proxy res) 

else 

instance subty2 :: ( Subtype s1 f1 g 
                   , Subtype s2 f2 g 
                   ) 
                   => Subtype (Sum s1 s2) (f1 :+: f2) g
  where 
  inj'' _ (Coproduct (Left x))  = inj'' (Proxy :: Proxy s1) x 
  inj'' _ (Coproduct (Right x)) = inj'' (Proxy :: Proxy s2) x 
  prj'' _ x = case prj'' (Proxy :: Proxy s1) x of 
    Just y -> Just (left y) 
    _      -> case prj'' (Proxy :: Proxy s2) x of 
                Just z -> Just (right z) 
                _      -> Nothing

-- | subtype api ---------------------------------------------------------
--------------------------------------------------------------------------

inject :: forall s f g a 
        . GetStruc f g s 
       => Subtype s f g 
       => Dupl f Nil False 
       => f a 
       -> g a 
inject = inj'' (Proxy :: Proxy s)

project :: forall s f g a 
         . GetStruc f g s 
        => Subtype s f g 
        => Dupl f Nil False 
        => g a 
        -> Maybe (f a)
project = prj'' (Proxy :: Proxy s) 