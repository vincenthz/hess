{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Hess.Define
	(
	  pval
	-- selector operators
	, (.>), (.@), (.+), (=#)
	-- definitions operators
	, (?>), (@>), (@!>)
	) where

import Prelude hiding (span)
import Hess.Type

class PValue a where
	pval :: a -> PropValue

instance PValue String where
	pval = PropString

instance PValue Int where
	pval i = PropInt i

instance PValue Color where
	pval c = PropColor c

instance Integral a => PValue (a,Unit) where
	pval (i, u) = PropLength (Length (fromIntegral i, u))

instance PValue Length where
	pval l = PropLength l
	
instance PValue PropValue where
	pval = id

(.@) :: Selector -> Selector -> Selector
(.@) = Within

(.>) :: Selector -> Selector -> Selector
(.>) = Child

(.+) :: Selector -> Selector -> Selector
(.+) = Adjacent

(=#) :: PValue a => String -> a -> Property
(=#) k v = Property k (pval v) 

(?>) :: [Selector] -> [PseudoClass] -> ([Property] -> Definition)
(?>) s c = Def s c

(@>) :: ([Property] -> Definition) -> [Property] -> Definition
(@>) f p = f p

(@!>) :: [Selector] -> [Property] -> Definition
(@!>) s p = s ?> [] @> p
