module Expr.Op where 

import Prelude

import Data.Maybe
import Data.Newtype
import Data.Tuple 

import Fix

import Type.Data.Boolean

import Typelevel.Het
import Typelevel.Subtype

import Type.Equality

import Typelevel.Context

type BinOp f exp 
  = SubT f exp 
 => Fix exp 
 -> Fix exp 
 -> Fix exp  

type TriOp f exp 
  = SubT f exp 
 => Fix exp 
 -> Fix exp 
 -> Fix exp 
 -> Fix exp

class Functor f <= BinaryOp f 
  where
  larg :: forall a. f a -> a  
  rarg :: forall a. f a -> a 

data FProxy (f :: Type -> Type) = FProxy 

 