-- | The compiler for Whitespace.
-- the only way compilation can fail
-- is due to label errors.
module Language.Whitespace.Compiler where

import Data.Word
import Language.Whitespace
import qualified Language.Greyspace as G
import qualified Language.Greyspace.Compiler as GC
import LLVM.AST

-- We don't actually use this in our program, but it's
-- a useful function to have available

-- | Compile the program to a LLVM Module
compile :: String -> Maybe Word64 -> Maybe Word64
        -> Program -> Either LabelError Module
compile f ss hs prog = (GC.compile f ss hs . G.toGreyspace)
  <$> greyspacePreProcess prog
