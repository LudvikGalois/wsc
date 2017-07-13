module Language.Greyspace where

import qualified Language.Whitespace as W
import Language.Whitespace (Label)
import Numeric.Natural

-- | A program is a list of blocks
type Program = [Block]

-- | A block is a label that starts it, followed by the instructions
-- to do followed by the method of branching
data Block = Block {label :: Label, stmts :: [Statement], term :: Terminator}
  deriving (Eq,Show)

-- | Currently most of these are unused, but they may be in the
-- future. The direct correspondents with whitespace are used
data Statement = Add
               | Sub
               | Mul
               | Mod
               | Div
               | AddI Integer
               | SubI Integer
               | MulI Integer
               | ModI Integer
               | DivI Integer
               | Load
               | LoadI Integer
               | Store
               | StoreI Integer
               | StoreII Integer Integer
               | Push Integer
               | Pop
               | Peek Integer
               | Slide Integer
               | Shuffle
               | Swap
               | Dup
               | ReadNum
               | PrintNum
               | ReadChar
               | PrintChar
  deriving (Eq,Show)

-- | Things that can terminate a Greyspace block
-- In retrospect treating a call as a branch was probably
-- unnecessary, but it can't hurt
data Terminator = Exit | Call Label Label | Jump Label | JumpZ Label Label
                | JumpN Label Label | Ret
  deriving (Eq,Show)

-- | Convert a pre-processed whitespace program into greyspace
toGreyspace :: W.GPP -> Program
toGreyspace = (\x -> zipWith wBlock2gBlock x (tail x ++ [[]]))
  . wBlocks . W.getProgram

-- | Split into blocks
wBlocks :: W.Program -> [W.Program]
wBlocks = wBlocks_ id
  where
    wBlocks_ acc [] = []
    wBlocks_ acc (op@(W.ControlOp _):xs) = acc [op] : wBlocks_ id xs
    wBlocks_ acc (x:xs) = wBlocks_ (acc . (x:)) xs

-- | Convert a "whitespace block" into an actual `Block`
-- the second argument is the following block so we can
-- link conditional jumps and call
wBlock2gBlock :: W.Program -> W.Program -> Block
wBlock2gBlock (W.Label n:p1) p2 = Block n instrs term
  where
    instrs = map toGrey (init p1)
    term = case (\(W.ControlOp x) -> x) $ last p1 of
      W.Ret -> Ret
      W.Jump n -> Jump n
      W.Exit -> Exit
      W.Call n -> Call n (addrOf p2)
      W.JumpZ n -> JumpZ n (addrOf p2)
      W.JumpN n -> JumpN n (addrOf p2)

-- | Get the label of a "whitespace block"
addrOf :: W.Program -> Label
addrOf (W.Label x:_) = x
addrOf _ = error "addrOf: Bad conversion into greyspace"

-- | Convert non-flow-control statements between whitespace and greyspace
toGrey :: W.Statement -> Statement
toGrey x = case x of
  W.ControlOp{} -> error "toGrey: Bad conversion into greyspace"
  W.Label{} -> error "toGrey: Bad conversion into greyspace"
  W.ArithOp op -> case op of
    W.Add -> Add
    W.Sub -> Sub
    W.Mul -> Mul
    W.Mod -> Mod
    W.Div -> Div
  W.StackOp op -> case op of
    W.Push n -> Push n
    W.Pop -> Pop
    W.Slide n -> Slide n
    W.Peek n -> Peek n
    W.Dup -> Dup
    W.Swap -> Swap
  W.HeapOp op -> case op of
    W.Store -> Store
    W.Load -> Load
  W.IOOp op -> case op of
    W.PrintNum -> PrintNum
    W.ReadNum -> ReadNum
    W.PrintChar -> PrintChar
    W.ReadChar -> ReadChar
