module Expr.Term where 

import Fix

import Type.Data.Boolean
import Typelevel.Het
import Typelevel.Subtype

type Term1 f a exp
  = SubT f exp => a -> Fix exp 

type Term2 f a b exp
  = SubT f exp => a -> b -> Fix exp 

type Term3 f a b c exp
  = SubT f exp => a -> b -> c -> Fix exp 

type Term4 f a b c d exp
  = SubT f exp => a -> b -> c -> d -> Fix exp 

type Term5 f a b c d e exp
  = SubT f exp => a -> b -> c -> d -> e -> Fix exp 