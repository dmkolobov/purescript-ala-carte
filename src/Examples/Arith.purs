module Examples.Arith where

import Prelude

import Data.Either (Either(..))
import Data.Functor.Coproduct
import Data.Functor.Coproduct.Syntax

import Expr.Term 
import Expr.Op 

import Fix
import Typelevel.Subtype 

import Type.Prelude

data Val a = ValF Int 
data Add a = AddF a a 
data Mul a = MulF a a  

derive instance valFunctor :: Functor Val 
derive instance addFunctor :: Functor Add 
derive instance mulFunctor :: Functor Mul

instance showVal :: Show (Val a) 
  where 
  show (ValF x) = show x
 
instance showAdd :: Show a => Show (Add a)
  where 
  show (AddF x y) = show x <> "+" <> show y 

instance showMul :: Show a => Show (Mul a)
  where 
  show (MulF x y) = show x <> "*" <> show y 

-- | Evaluation to integers
---------------------------

class ArithEval f where 
  evalArithA :: f Int -> Int 

instance evalArithVal :: ArithEval Val where 
  evalArithA (ValF x) = x 

instance evalArithAdd :: ArithEval Add where 
  evalArithA (AddF x y) = x + y 

instance evalArithMul :: ArithEval Mul where 
  evalArithA (MulF x y) = x * y

instance evalArithCoproduct :: ( ArithEval f , ArithEval g ) => ArithEval (f :+: g) 
  where 
  evalArithA (Coproduct (Left fa))  = evalArithA fa 
  evalArithA (Coproduct (Right ga)) = evalArithA ga

evalArith :: forall f
           . Functor f 
          => ArithEval f 
          => Fix f 
          -> Int 
evalArith = fold evalArithA

-- | Smart constructors
-----------------------

val :: forall exp. Term1 Val Int exp 
val x = Fix (inject (ValF x))

add :: forall exp. BinOp Add exp 
add x y = Fix (inject (AddF x y))

mul :: forall exp. BinOp Mul exp 
mul x y = Fix (inject (MulF x y))

e1 :: Fix Val 
e1 = val 42

e2 :: Fix (Val :+: Add) 
e2 = val 2 `add` val 3

e3 :: Fix (Val :+: Add :+: Mul)
e3 = val 2 `add` (val 5 `mul` val 3)

e4 :: Fix (Val :+: Add :+: Mul) 
e4 = fold (Fix <<< inject) e2 `add` e3