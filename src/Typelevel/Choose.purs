module Typelevel.Choose where

-- | Represents a choice between two search results `l` and `r`, which 
-- are themselves choices. 
class Choose l r pos | l r -> pos  

-- first we define some void data types corresponding positions in a coproduct.   

data Here  -- found here
data L pos -- found in the left branch
data R pos -- found in the right branch

-- then we define void data types corresponding to the possible results of membership 
-- testing

data Found pos  -- we found something at the given position 
data NotFound   -- we found nothing 
data Ambiguous  -- we found too many things and the choice between them is ambiguous

-- ambiguity arises from two different results when searching the left and right summands
instance chooseBoth :: Choose (Found l) (Found r) Ambiguous else 

-- ambiguity in the left search leads to ambiguity for the search as a whole
instance chooseAmbL :: Choose Ambiguous r Ambiguous else 

-- ambiguity in the right search leads to ambiguity for the search as a whole
instance chooseAmbR :: Choose l Ambiguous Ambiguous else

-- the search over the left summand returns a result
instance chooseL :: Choose (Found pos) r (Found (L pos)) else

-- the search over the right summand returns a result
instance chooseR :: Choose l (Found pos) (Found (R pos)) else

-- no results found
instance chooseM :: Choose l r NotFound