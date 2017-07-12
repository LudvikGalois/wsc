-- | The compiler for Whitespace.
-- the only way compilation can fail
-- is due to label errors.
module Language.Whitespace.Compiler where

import Language.Whitespace
import qualified Language.Greyspace as G
import qualified Language.Greyspace.Compiler as GC
import LLVM.AST

-- | Compile the program to a LLVM Module
compile :: Program -> Either LabelError Module
compile prog = (GC.compile "whitepsace" Nothing Nothing . G.toGreyspace) <$> greyspacePreProcess prog
