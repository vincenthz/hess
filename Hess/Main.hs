{-# LANGUAGE DeriveDataTypeable #-}
module Hess.Main
	( defaultMain
	, module Hess.Type
	, module Hess.Generate
	, module Hess.Define
	, module Hess.Selector
	, module Hess.Property
	) where

import System.Console.CmdArgs
import Hess.Type
import Hess.Generate
import Hess.Define
import Hess.Selector
import Hess.Property

data Opts = Opts
	{ out :: FilePath
	} deriving (Show,Eq,Data,Typeable)

outFlags :: Data val => val -> val
outFlags x = x &= help "Output file" &= typFile

opts :: Opts
opts = Opts
	{ out = outFlags "screen.css"
	}
	&= help "Generate a CSS file from a stylesheet"

defaultMain :: [Definition] -> IO ()
defaultMain defs = do
	o <- cmdArgs opts
	let content = generate defs
	writeFile (out o) content
