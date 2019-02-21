module Expr.Op where 

import Fix

import Type.Data.Boolean

import Typelevel.Het
import Typelevel.Subtype

type BinOp f exp 
  = forall s dup z 
  . GetStruc f exp s 
 => Subtype s f exp 
 => Dupl f Nil False 
 => Fix exp 
 -> Fix exp 
 -> Fix exp  

type TriOp f exp 
  = forall s dup z 
  . GetStruc f exp s 
 => Subtype s f exp 
 => Dupl f Nil False 
 => Fix exp 
 -> Fix exp 
 -> Fix exp 
 -> Fix exp
