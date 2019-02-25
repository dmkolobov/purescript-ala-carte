module Typelevel.Context where

type With2 (f :: Type -> Type) 
           (g :: Type -> Type) 
           z 
           = f (g z) 

type With3 (f :: Type -> Type) 
           (g :: Type -> Type) 
           (h :: Type -> Type) 
           z 
           = f (g (h z))
