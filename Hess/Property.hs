{-# LANGUAGE FlexibleInstances #-}
module Hess.Property
	( BorderStyle(..)
	-- margin properties
	, margin, margin4
	, marginLeft, marginTop, marginRight, marginBottom
	-- padding properties
	, padding, padding4
	, paddingLeft, paddingTop, paddingRight, paddingBottom
	-- dimension properties
	, height, width
	, maxHeight, maxWidth, minHeight, minWidth
	-- font properties
	, font
	, fontFamily, fontSize, fontStyle, fontVariant, fontWeight
	-- list properties
	, listStyle
	, listStyleImage
	, listStylePosition
	, listStyleType
	-- text properties
	, textColor
	, textDecoration
	, textWeight
	, textAlign
	-- background properties
	, backgroundAttachment
	, backgroundColor
	, backgroundImage
	, backgroundPosition
	, backgroundRepeat
	-- border properties
	, border, borderLeft, borderTop, borderRight, borderBottom
	-- positioning properties (left top right bottom)
	, posLeft, posTop, posRight, posBottom

	, display, clear, zIndex, position, overflow, float, visibility
	) where

import Hess.Type
import Hess.Define
import Data.List (intercalate)

data BorderStyle =
	  BorderStyleNone
	| BorderStyleHidden
	| BorderStyleDotted
	| BorderStyleDashed
	| BorderStyleSolid
	| BorderStyleDouble
	| BorderStyleGroove
	| BorderStyleRidge
	| BorderStyleInset
	| BorderStyleOutset
	deriving (Eq)

instance Show BorderStyle where
	show BorderStyleNone   = "none"
	show BorderStyleHidden = "hidden"
	show BorderStyleDotted = "dotted"
	show BorderStyleDashed = "dashed"
	show BorderStyleSolid  = "solid"
	show BorderStyleDouble = "double"
	show BorderStyleGroove = "groove"
	show BorderStyleRidge  = "ridge"
	show BorderStyleInset  = "inset"
	show BorderStyleOutset = "outset"

data Display =
	  DisplayBlock
	| DisplayInline
	deriving (Eq)

instance Show Display where
	show DisplayBlock  = "block"
	show DisplayInline = "inline"

class Lengthable a where
	toLength :: a -> Length

instance Integral a => Lengthable (a, Unit) where
	toLength (i, u) = Length (fromIntegral i, u)

{- this unforunately requires UndecidableInstances, so just define for int below
instance Integral a => Lengthable a where
	toLength x = Length (fromIntegral x, Nu)
-}
instance Lengthable Int where
	toLength x = Length (x, Nu)

instance Lengthable Integer where
	toLength x = Length (fromIntegral x, Nu)

instance Lengthable Length where
	toLength x = x

margin :: (Lengthable a, Lengthable b, Lengthable c, Lengthable d) => a -> b -> c -> d -> Property
margin a b c d = Property "margin" $ pval v
	where v = intercalate " " $ map (show) [toLength a,toLength b,toLength c,toLength d]

margin4 :: (Lengthable a) => a -> Property
margin4 a = margin a a a a

marginLeft, marginTop, marginRight, marginBottom :: Lengthable a => a -> Property
marginLeft   = Property "margin-left" . pval . toLength
marginTop    = Property "margin-top" . pval . toLength
marginRight  = Property "margin-right" . pval . toLength
marginBottom = Property "margin-bottom" . pval . toLength

padding :: (Lengthable a, Lengthable b, Lengthable c, Lengthable d) => a -> b -> c -> d -> Property
padding a b c d = Property "padding" $ pval v
	where v = intercalate " " $ map (show) [toLength a,toLength b,toLength c,toLength d]

padding4 :: (Lengthable a) => a -> Property
padding4 a = padding a a a a

paddingLeft, paddingTop, paddingRight, paddingBottom :: Lengthable a => a -> Property
paddingLeft   = Property "padding-left" . pval . toLength
paddingTop    = Property "padding-top" . pval . toLength
paddingRight  = Property "padding-right" . pval . toLength
paddingBottom = Property "padding-bottom" . pval . toLength

textColor :: Color -> Property
textColor = Property "color" . pval

textDecoration :: String -> Property
textDecoration = Property "text-decoration" . pval

textWeight :: String -> Property
textWeight = Property "text-weight" . pval

textAlign :: String -> Property
textAlign = Property "text-align" . pval

backgroundColor :: Color -> Property
backgroundColor = Property "background-color" . pval

backgroundAttachment :: String -> Property
backgroundAttachment = Property "background-attachment" . pval

backgroundImage :: String -> Property
backgroundImage = Property "background-image" . pval

backgroundPosition :: String -> Property
backgroundPosition = Property "background-position" . pval

backgroundRepeat :: String -> Property
backgroundRepeat = Property "background-repeat" . pval

border, borderLeft, borderTop, borderRight, borderBottom :: Lengthable a => a -> BorderStyle -> Color -> Property
border len bs color       = Property "border" $ pval $ intercalate " " [show $ toLength len, show bs, show color]
borderLeft len bs color   = Property "border-left" $ pval $ intercalate " " [show $ toLength len, show bs, show color]
borderTop len bs color    = Property "border-top" $ pval $ intercalate " " [show $ toLength len, show bs, show color]
borderRight len bs color  = Property "border-right" $ pval $ intercalate " " [show $ toLength len, show bs, show color]
borderBottom len bs color = Property "border-bottom" $ pval $ intercalate " " [show $ toLength len, show bs, show color]

height, width, maxHeight, maxWidth, minHeight, minWidth :: Lengthable a => a -> Property
height    = Property "height" . pval . toLength
width     = Property "width" . pval . toLength
maxHeight = Property "max-height" . pval . toLength
maxWidth  = Property "max-width" . pval . toLength
minHeight = Property "min-height" . pval . toLength
minWidth  = Property "min-width" . pval . toLength

font, fontFamily, fontSize, fontStyle, fontVariant, fontWeight :: String -> Property
font        = Property "font" . pval
fontFamily  = Property "font-family" . pval
fontSize    = Property "font-size" . pval
fontStyle   = Property "font-style" . pval
fontVariant = Property "font-variant" . pval
fontWeight  = Property "font-weight" . pval

listStyle :: String -> Property
listStyle = undefined

listStyleImage :: String -> Property
listStyleImage = Property "list-style-image" . pval

listStylePosition :: String -> Property
listStylePosition = Property "list-style-position" . pval

listStyleType :: String -> Property
listStyleType = Property "list-style-type" . pval

posLeft, posTop, posRight, posBottom :: Lengthable a => a -> Property
posLeft   = Property "left" . pval . toLength
posTop    = Property "top" . pval . toLength
posRight  = Property "right" . pval . toLength
posBottom = Property "bottom" . pval . toLength

display :: String -> Property
display = Property "display" . pval

clear :: String -> Property
clear = Property "clear" . pval

zIndex :: Int -> Property
zIndex = Property "z-index" . pval . show

position :: String -> Property
position = Property "position" . pval

overflow :: String -> Property
overflow = Property "overflow" . pval

float :: String -> Property
float = Property "float" . pval

visibility :: String -> Property
visibility = Property "float" . pval
