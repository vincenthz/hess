module Hess.Selector
	(
	-- selectors
	  body,h1,h2,h3,h4,h5,h6
	, a,img,table,thead,tr,td,span,hr
	, ul,ol,li
	, b, em
	, pre,code,blockquote
	, input, textarea
	) where

import Prelude ()
import Hess.Type

body :: Selector
body = Type "body"

h1,h2,h3,h4,h5,h6 :: Selector
h1   = Type "h1"
h2   = Type "h2"
h3   = Type "h3"
h4   = Type "h4"
h5   = Type "h5"
h6   = Type "h6"

a,img,table,thead,tr,td,span,hr :: Selector
a     = Type "a"
img   = Type "img"
table = Type "table"
thead = Type "thead"
tr    = Type "tr"
td    = Type "td"
span  = Type "span"
hr    = Type "hr"

ul,ol,li :: Selector
ul   = Type "ul"
ol   = Type "ol"
li   = Type "li"

b, em :: Selector
b    = Type "b"
em   = Type "em"

pre,code,blockquote :: Selector
pre        = Type "pre"
code       = Type "code"
blockquote = Type "blockquote"

input,textarea :: Selector
input    = Type "input"
textarea = Type "textarea"
