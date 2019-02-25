module Typelevel.Match where

import Control.Monad 

import Data.Function
import Data.Functor.Coproduct.Syntax
import Data.Maybe

import Fix

import Type.Prelude 
import Typelevel.Subtype

import Data.Unit 
import Data.Newtype 

import Expr.Op

import Data.List ((:), List(..))

import Data.Foldable

import Matryoshka.Class.Recursive 

-- the kind of type-level match expressions

foreign import kind Match

foreign import data Any  :: Match
foreign import data In :: (Type -> Type) -> Match 
foreign import data Bin  :: (Type -> Type) -> Match -> Match -> Match

data Pat (m :: Match) = Case

class Matches (t :: Type) (m :: Match) c z | m c -> t z
  where 
  matchCase :: Pat m -> c -> t -> Maybe z

instance matchesAny :: Matches t Any (t -> z) z 
  where 
  matchCase _ f = Just <<< f

instance matchesIn :: (Recursive t l, SubT f l) => Matches t (In f) (f t -> z) z
  where 
  matchCase _ f texp = f <$> prj (project texp :: l t)

instance matchesBin :: ( Recursive t l 
                       , SubT f l 
                       , BinaryOp f 
                       , Matches t ml c cr
                       , Matches t mr cr z
                       ) => Matches t (Bin f ml mr) c z where 
  matchCase _ f texp = do 
                              op <- prj (project texp :: l t) :: Maybe (f t)
                              cr <- matchCase (Case :: Pat ml) f (larg op) 
                              matchCase (Case :: Pat mr) cr (rarg op)

infixl 1 matchCase as :=

match :: forall t f a. Foldable f => t
                                  -> f (t -> Maybe a) 
                                  -> Maybe a
match = findMap <<< flip ($)


