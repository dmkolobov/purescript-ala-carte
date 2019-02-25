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

import Data.NaturalTransformation

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

type Subsumption f g a = SubT f g => a
infixl 4 type Subsumption as :<:

type Has2 l f g a = (f :<: l) ((g :<: l) a)

{-}
inject :: forall s f g a. Subsumed s f g => f a -> g a 
inject = inj'' (Proxy :: Proxy s)

project :: forall s f g a. Subsumed s f g => g a -> Maybe (f a)
project = prj'' (Proxy :: Proxy s)  -}

class ( GetStruc f g s , Subtype s f g, Dupl f Nil False ) <= Subsumed s f g | f g -> s

instance subsumed :: (GetStruc f g s, Subtype s f g, Dupl f Nil False) => Subsumed s f g

type Isomorphism f g a = SubT f g => SubT g f => a

infixl 4 type Isomorphism as :~: 

split :: forall f l r a b. (f :~: l :+: r) ((l a -> b) -> (r a -> b) -> f a -> b) 
split fl fr x = case inj x of Coproduct (Left y)  -> fl y 
                              Coproduct (Right y) -> fr y

class SubT (f :: Type -> Type) (g :: Type -> Type) 
  where 
  inj :: forall a. f a -> g a 
  prj :: forall a. g a -> Maybe (f a)

instance subT :: ( GetStruc f g s , Subtype s f g , Dupl f Nil False ) => SubT f g
  where
  inj = inj'' (Proxy :: Proxy s) 
  prj = prj'' (Proxy :: Proxy s)