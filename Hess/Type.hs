module Hess.Type
	( Selector(..)
	, PseudoClass(..)
	, PropValue(..)
	, Property(..)
	, Definition(..)
	, Color(..)
	, Length(..)
	, Unit(..)
	) where

import Numeric (showHex)
import Data.Word

data Selector =
	  Class String
	| ID String
	| Type String
	| All
	| Within Selector Selector
	| Child Selector Selector
	| Adjacent Selector Selector
	| OfClass Selector String
	deriving (Eq)

data PseudoClass =
	  FirstChild
	| Link
	| Visited
	| Hover
	| Active
	| Focus
	| FirstLine
	| FirstLetter
	| Before
	| After
	| Lang String
	deriving (Eq)

data Unit = Nu | Em | Px
	deriving (Eq)

data Color = Color Word8 Word8 Word8
	deriving (Eq)

data Length =
	  Length (Int, Unit)
	| Auto
	| Inherit
	deriving (Eq)

instance Show Length where
	show (Length (i,ut)) = show i ++ show ut
	show Auto            = "auto"
	show Inherit         = "inherit"

data PropValue =
	  PropString String
	| PropInt Int
	| PropLength Length
	| PropColor Color
	deriving (Eq)

data Property = Property String PropValue
	deriving (Eq)

data Definition = Def [Selector] [PseudoClass] [Property]
	deriving (Eq)

instance Show Color where
	show (Color r g b) = concat ["#",rs,gs,bs]
		where
			rs = (if r < 0x10 then "0" else "") ++ showHex r ""
			gs = (if g < 0x10 then "0" else "") ++ showHex g ""
			bs = (if b < 0x10 then "0" else "") ++ showHex b ""

instance Show Unit where
	show Nu = ""
	show Em = "em"
	show Px = "px"

instance Show PropValue where
	show (PropString s) = s
	show (PropInt i)    = show i
	show (PropLength l) = show l
	show (PropColor c)  = show c

instance Show Selector where
	show (Class s)        = "." ++ s
	show (ID s)           = "#" ++ s
	show (Type s)         = s
	show (All)            = "*"
	show (Within s1 s2)   = show s1 ++ " " ++ show s2
	show (Child s1 s2)    = show s1 ++ ">" ++ show s2
	show (Adjacent s1 s2) = show s1 ++ "+" ++ show s2
	show (OfClass s c)    = show s ++ "." ++ c

instance Show PseudoClass where
	show FirstChild  = "first-child"
	show FirstLine   = "first-line"
	show FirstLetter = "first-letter"
	show Link        = "link"
	show Visited     = "visited"
	show Hover       = "hover"
	show Active      = "active"
	show Focus       = "focus"
	show Before      = "before"
	show After       = "after"
	show (Lang s)    = "lang(" ++ s ++ ")"

instance Show Property where
	show (Property k v) = k ++ ": " ++ show v
