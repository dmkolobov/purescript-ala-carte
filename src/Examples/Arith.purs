module Examples.Arith where

import Prelude

import Data.Either (Either(..))
import Data.Eq
import Data.Functor.Coproduct
import Data.Functor.Coproduct.Syntax
import Data.Maybe 
import Data.List hiding (fold)
 
import Data.Newtype 

import Data.Tuple

import Expr.Term 
import Expr.Op 

import Fix

import Typelevel.Context
import Typelevel.Match
import Typelevel.Subtype 

import Type.Prelude

data Val a = ValF Int 
data Add a = AddF a a 
data Mul a = MulF a a  
data Exp a = ExpF a a 

derive instance valFunctor :: Functor Val 
derive instance addFunctor :: Functor Add 
derive instance mulFunctor :: Functor Mul
derive instance expFunctor :: Functor Exp

instance showVal :: Show (Val a) 
  where 
  show (ValF x) = show x
 
instance showAdd :: Show a => Show (Add a)
  where 
  show (AddF x y) = show x <> "+" <> show y 

instance showMul :: Show a => Show (Mul a)
  where 
  show (MulF x y) = show x <> "*" <> show y 

instance binaryAdd :: BinaryOp Add
  where 
  larg (AddF x _) = x 
  rarg (AddF _ x) = x 

instance binaryMul :: BinaryOp Mul 
  where 
  larg (MulF x _) = x 
  rarg (MulF _ x) = x

instance binaryExp :: BinaryOp Exp 
  where 
  larg (ExpF x _) = x 
  rarg (ExpF _ x) = x

-- | Evaluation to integers
---------------------------

pow :: Int -> Int -> Int -> Int
pow _ 0 acc = acc
pow x y acc = pow x (y - 1) (x * acc) 

class ArithEval f where 
  evalArithA :: f Int -> Int 

instance evalArithVal :: ArithEval Val where 
  evalArithA (ValF x) = x 

instance evalArithAdd :: ArithEval Add where 
  evalArithA (AddF x y) = x + y 

instance evalArithMul :: ArithEval Mul where 
  evalArithA (MulF x y) = x * y

instance evalExp :: ArithEval Exp where 
  evalArithA (ExpF x y) = (x `pow` y) 1

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

-- | Equality -----------------------------------------------------------------
-------------------------------------------------------------------------------

instance equalVal :: Eq1 Val
  where 
  eq1 (ValF x) (ValF y) = x == y

instance equalAdd :: Eq1 Add 
  where 
  eq1 (AddF a b) (AddF c d) = a == c && b == d

instance equalMul :: Eq1 Mul 
  where 
  eq1 (MulF a b) (MulF c d) = a == c && b == d 

instance equalExp :: Eq1 Exp 
  where 
  eq1 (ExpF a b) (ExpF c d) = a == c && b == d


-- | Smart constructors
-----------------------

type BinExp (l :: Match) (r :: Match) = Bin Exp l r
type BinMul (l :: Match) (r :: Match) = Bin Mul l r
type BinAdd (l :: Match) (r :: Match) = Bin Add l r

infixl 8 type BinExp as |^|
infixr 7 type BinMul as |*| 
infixr 6 type BinAdd as |+|

gdistr :: forall f g l
        . Functor l 
       => BinaryOp f 
       => BinaryOp g 
       => SubT f l 
       => SubT g l 
       => FProxy f -- this operation 
       -> FProxy g -- distributes over this operation
       -> (Fix l -> Fix l -> Fix l) -- smart `f` op constuctor
       -> (Fix l -> Fix l -> Fix l) -- smart `g` op constructor
       -> Fix l                     -- expression to match
       -> Maybe (Fix l)             -- possible result of match
gdistr _ _ mkF mkG exp 
  = exp `match` 
      [ (Case :: Pat (Bin f (Bin g Any Any) Any)) := \ a b x -> (a `mkF` x) `mkG` (b `mkF` x)
      , (Case :: Pat (Bin f Any (Bin g Any Any))) := \ x a b -> (x `mkF` a) `mkG` (x `mkF` b)
      ]

gidentity :: forall f v l 
           . Functor l 
          => BinaryOp f 
          => SubT f l 
          => SubT v l 
          => FProxy f 
          -> (v (Fix l) -> Boolean) -- is this the identity element? 
          -> Fix l 
          -> Maybe (Fix l)
gidentity _ isId exp 
  = join
  ( exp `match` 
  [ (Case :: Pat (Bin f (In v) Any))    := \v a -> if isId v then Just (a :: Fix l) else Nothing
  , (Case :: Pat (Bin f Any    (In v))) := \a v -> if isId v then Just (a :: Fix l) else Nothing
  ]) 

mulId :: forall l 
       . Functor l 
      => SubT Mul l 
      => SubT Val l 
      => Fix l 
      -> Maybe (Fix l) 
mulId = gidentity (FProxy :: FProxy Mul) (\(ValF x) -> x == 1)

addId :: forall l 
       . Functor l 
      => SubT Add l 
      => SubT Val l 
      => Fix l 
      -> Maybe (Fix l) 
addId = gidentity (FProxy :: FProxy Add) (\(ValF x) -> x == 0)

-- | Matches expressions like `(a + b) * c`` in a language with multiplication and 
-- addition, turning them into expressions of the form `a * c + b * c`
mulDist :: forall l
         . Functor l 
        => SubT Add l 
        => SubT Mul l 
        => Fix l 
        -> Maybe (Fix l) 
mulDist = gdistr (FProxy :: FProxy Mul)
                 (FProxy :: FProxy Add) 
                 mul 
                 add

-- | Matches expressions like `(a * b) ^ c` in a language with exponentiation and 
-- multiplication, turning them into expressions of the form `a ^ c * b ^ c``
expDist :: forall l 
         . Functor l 
        => SubT Exp l 
        => SubT Mul l 
        => Fix l 
        -> Maybe (Fix l) 
expDist = gdistr (FProxy :: FProxy Exp) 
                 (FProxy :: FProxy Mul) 
                 exp 
                 mul

val :: forall exp. SubT Val exp => Int -> Fix exp 
val x = Fix (inj (ValF x))

add :: forall exp. SubT Add exp => Fix exp -> Fix exp -> Fix exp  
add x y = Fix (inj (AddF x y))

mul :: forall exp. SubT Mul exp => Fix exp -> Fix exp -> Fix exp  
mul x y = Fix (inj (MulF x y))

exp :: forall exp. SubT Exp exp => Fix exp -> Fix exp -> Fix exp 
exp x y = Fix (inj (ExpF x y))

e1 :: Fix Val 
e1 = val 42

e2 :: Fix (Val :+: Add) 
e2 = val 2 `add` val 3

e3 :: Fix (Val :+: Add :+: Mul)
e3 = val 2 `add` (val 5 `mul` val 3)

e4 :: Fix (Val :+: Add :+: Mul) 
e4 = fold (Fix <<< inj) e2 `add` e3 

e5 :: Fix (Val :+: Add) 
e5 = val 0 `add` (val 2 `add` val 3)

class Print f where 
  pp :: f String -> String

instance printAdd :: Print Add where 
  pp (AddF x y) = "(" <> x <> " + " <> y <> ")"
  
instance printMul :: Print Mul where 
  pp (MulF x y) = "(" <> x <> " * " <> y <> ")"

instance printExp :: Print Exp where 
  pp (ExpF x y) = "(" <> x <> " ^ " <> y <> ")"

instance printVal :: Print Val where 
  pp (ValF x) = show x

instance printCoproduct :: (Print f, Print g) => Print (f :+: g) 
  where 
  pp (Coproduct (Left fs))  = pp fs 
  pp (Coproduct (Right fs)) = pp fs

pretty :: forall f. Functor f => Print f => Fix f -> String 
pretty = fold pp

e6 :: Fix (Val :+: Add :+: Mul) 
e6 = val 4 `mul` (val 2 `add` val 3)

e7 :: Fix (Val :+: Add :+: Mul) 
e7 = (val 2 `add` val 3) `mul` val 4

e8 :: Fix (Val :+: Add :+: Mul :+: Exp) 
e8 = (val 2 `exp` val 3) `mul` (val 2 `exp` val 4)

e9 :: Fix (Val :+: Add :+: Mul :+: Exp) 
e9 = (val 2 `exp` val 3) `mul` (val 5 `exp` val 4)

e10 :: Fix (Val :+: Add :+: Mul :+: Exp) 
e10 = (val 2 `exp` val 3) `exp` (val 5 `add` val 6)


e11 :: Fix (Val :+: Add :+: Mul :+: Exp)
e11 = (val 2 `mul` val 3) `exp` val 5