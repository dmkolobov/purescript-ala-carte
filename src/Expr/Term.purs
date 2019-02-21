module Expr.Term where 

import Fix

import Type.Data.Boolean
import Typelevel.Het
import Typelevel.Subtype

type Term1 f a exp
  = forall s dup z 
  . GetStruc f exp s 
 => Subtype s f exp 
 => Dupl f Nil False => a -> Fix exp 

type Term2 f a b exp
  = forall s dup z 
  . GetStruc f exp s 
 => Subtype s f exp 
 => Dupl f Nil False => a -> b -> Fix exp 

type Term3 f a b c exp
  = forall s dup z 
  . GetStruc f exp s 
 => Subtype s f exp 
 => Dupl f Nil False => a -> b -> c -> Fix exp 

type Term4 f a b c d exp
  = forall s dup z 
  . GetStruc f exp s 
 => Subtype s f exp 
 => Dupl f Nil False => a -> b -> c -> d -> Fix exp 

type Term5 f a b c d e exp
  = forall s dup z 
  . GetStruc f exp s 
 => Subtype s f exp 
 => Dupl f Nil False => a -> b -> c -> d -> e -> Fix exp 