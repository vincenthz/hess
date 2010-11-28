module Hess.Generate
	( generate
	) where

import Data.List
import Hess.Type

generateDef :: Definition -> String
generateDef (Def sels pseudoclasses properties) = ss ++ pcs ++ " { " ++ props ++ " }"
	where
		ss    = intercalate ", " $ map show sels
		props = intercalate "; " $ map show properties
		pcs   =
			if length pseudoclasses > 0
				then ":" ++ (intercalate ":" $ map show pseudoclasses)
				else ""

generate :: [Definition] -> String
generate = intercalate "\n" . map generateDef
