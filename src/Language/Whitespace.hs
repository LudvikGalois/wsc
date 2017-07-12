{-# Language BangPatterns #-}
-- | A library for the AST of the Whitespace programming
-- language.
module Language.Whitespace
  ( Statement (..), StackOp (..), ArithOp (..)
  , HeapOp (..), IOOp (..), ControlOp (..)
  , Label, Program, LabelError (..), PPWhitespace (..), showLabel, reLabel
  , GPP, getProgram, greyspacePreProcess
  ) where

import Numeric.Natural
import qualified Data.Map.Strict as M
import Data.Bits
import Data.Coerce

-- | We represent labels as natural numbers, not strings as the spec
-- suggests to make them easier to work with
type Label = Natural

-- | A `Program` is nothing more than a list of `Statement`
type Program = [Statement]
type Block = [Statement]

-- | A statement in the Whitespace language. We differ from the
-- spec by not having labels in `ControlOp`
data Statement = StackOp StackOp
               | ArithOp ArithOp
               | HeapOp HeapOp
               | IOOp IOOp
               | ControlOp ControlOp
               | Label Label
  deriving (Eq, Ord, Show)

-- | A stack operation, including the undocumented commands of
-- slide, peek and shuffle. They are however in the reference
-- implementation which is why we include them (and they also
-- make writing programs much easier)
data StackOp = Push Integer
             | Pop
             | Swap
             | Dup
             | Slide Integer
             | Peek Integer
             | Shuffle
  deriving (Eq, Ord, Show)

-- | An arithmetic operation
data ArithOp = Add
             | Sub
             | Mul
             | Div
             | Mod
  deriving (Eq, Ord, Show)

-- | A heap operation
data HeapOp = Load
            | Store
  deriving (Eq, Ord, Show)

-- | The IO operations
data IOOp = ReadNum
          | PrintNum
          | ReadChar
          | PrintChar
  deriving (Eq, Ord, Show)

-- | The control operations (wtihout labels)
data ControlOp = Jump Label
               | JumpN Label
               | JumpZ Label
               | Call Label
               | Ret
               | Exit
  deriving (Eq, Ord, Show)

-- | Whilst we leave most program analysis elsewhere
-- we include some tools for relabeling, and these can
-- fail
data LabelError = Repeated Label
                | NotFound Label
  deriving (Eq, Show)

-- | Things which are whitespace we can print
class PPWhitespace a where
  ppWhite :: a -> String

instance PPWhitespace a => PPWhitespace [a] where
  ppWhite = unlines . map ppWhite

instance PPWhitespace Statement where
  ppWhite (Label l) = showLabel l ++ ":" 
  ppWhite (StackOp s) = '\t':ppWhite s
  ppWhite (ArithOp s) = '\t':ppWhite s
  ppWhite (ControlOp s) = '\t':ppWhite s
  ppWhite (IOOp s) = '\t':ppWhite s
  ppWhite (HeapOp s) = '\t':ppWhite s

instance PPWhitespace IOOp where
  ppWhite ReadChar = "getc"
  ppWhite PrintChar = "putc"
  ppWhite ReadNum = "getn"
  ppWhite PrintNum = "putn"

instance PPWhitespace HeapOp where
  ppWhite Load = "load"
  ppWhite Store = "store"

instance PPWhitespace StackOp where
  ppWhite (Push n) = "push " ++ show n
  ppWhite Pop = "pop"
  ppWhite Dup = "dup"
  ppWhite Swap = "swap"
  ppWhite (Slide n) = "slide " ++ show n
  ppWhite (Peek n) = "peek " ++ show n
  ppWhite Shuffle = "shuffle"

instance PPWhitespace ArithOp where
  ppWhite Add = "add"
  ppWhite Mul = "mul"
  ppWhite Sub = "sub"
  ppWhite Div = "div"
  ppWhite Mod = "mod"

instance PPWhitespace ControlOp where
  ppWhite (Jump n) = "jump " ++ showLabel n
  ppWhite (JumpN n) = "jump_n " ++ showLabel n 
  ppWhite (JumpZ n) = "jump_z " ++ showLabel n
  ppWhite (Call n) = "call " ++ showLabel n
  ppWhite Ret = "ret"
  ppWhite Exit = "exit"

-- We're going to target LLVM, so it's useful to split this into control flow blocks
-- A block starts where another one ends, a block ends at a ControlOp, or right before
-- a label

blocks :: Program -> [Program]
blocks = filter (not . null) . blocks_ id
  where blocks_ acc [] = [acc []]
        blocks_ acc (ControlOp s:xs) = acc [ControlOp s] : blocks_ id xs
        blocks_ acc (x:xs)
          | Label l:_ <- xs = acc [x] : blocks_ id xs
          | otherwise = blocks_ (acc . (x:)) xs

-- Find the next free label. We stick 0 on the list in case it's
-- empty
freeLabel :: Program -> Natural
freeLabel = succ . maximum . (0:) . getLabels
  where getLabels (Label l:xs) = l : getLabels xs
        getLabels (_:xs) = getLabels xs
        getLabels [] = []

-- Link our blocks together with explicit control
-- instructions. We don't include the alternate
-- locations of conditional jumps, this is mainly
-- to demarcate boundraries since the expected target
-- is LLVM
linkBlock :: Natural -> [Program] -> [Program]
linkBlock n [] = [[Label n, ControlOp Exit]]
linkBlock n ([]:xs) = linkBlock n xs
linkBlock n [x] = case last x of
  ControlOp Exit -> [x]
  ControlOp _ -> [x,[Label n, ControlOp Exit]]
  _ -> [x ++ [ControlOp Exit]]
linkBlock n (x@(Label _:_):xs) = case last x of
  ControlOp _ -> x:linkBlock n xs
  _ -> case head xs of
    [] -> linkBlock n (x:tail xs)
    Label l:_ -> (x ++ [ControlOp $ Jump l]): linkBlock n xs
    s:xs' -> (x ++ [ControlOp $ Jump n]): linkBlock (n+1) ((Label n :s:xs'): tail xs)
linkBlock n (x:xs) = linkBlock (n+1) ((Label n:x):xs)

-- | Convert a label back to a String. This only makes sense if you've
-- used my parser to read in the program
showLabel :: Natural -> String
showLabel n = map (\p -> if p then 't' else 's') $ drop 1 $ reverse
  $ map (n `testBit` ) $ takeWhile (\x -> 2^x <= n) [0..]

-- | Make our labels consecutive numbers (e.g the program starts at
-- label 0, and every label is in sequential order).
-- Since this needs to do some analysis of the program,
-- it may fail
reLabel :: Program -> Either LabelError Program
reLabel p = do
  newIndices <- getIndices M.empty 0 p
  updateIndices newIndices p
  where
    getIndices m n [] = Right m
    getIndices !m n (Label x:xs) = case M.lookup x m of
      Nothing -> getIndices (M.insert x n m) (n+1) xs
      Just _ -> Left $ Repeated x
    getIndices !m n (_:xs) = getIndices m n xs
    updateIndices m [] = Right []
    updateIndices m (Label x:xs) = (Label (m M.! x) :) <$> updateIndices m xs
    updateIndices m (ControlOp (Jump x):xs) = case M.lookup x m of
      Just a -> (ControlOp (Jump a) :) <$> updateIndices m xs
      Nothing -> Left $ NotFound x
    updateIndices m (ControlOp (JumpZ x):xs) = case M.lookup x m of
      Just a -> (ControlOp (JumpZ a) :) <$> updateIndices m xs
      Nothing -> Left $ NotFound x
    updateIndices m (ControlOp (JumpN x):xs) = case M.lookup x m of
      Just a -> (ControlOp (JumpN a) :) <$> updateIndices m xs
      Nothing -> Left $ NotFound x
    updateIndices m (ControlOp (Call x):xs) = case M.lookup x m of
      Just a -> (ControlOp (Call a) :) <$> updateIndices m xs
      Nothing -> Left $ NotFound x
    updateIndices m (x:xs) = (x :) <$> updateIndices m xs

-- | A greyspace pre-processed program
newtype GPP = GPP Program
  deriving (Eq, Show)

-- | Extract a program after greyspace pre-processing
getProgram :: GPP -> Program
getProgram (GPP x) = x

-- | Greyspace is our target intermediate language - it actually has
-- more instruction than Whitespace, but the most important thing
-- is that like LLVM, it's grouped into blocks with explicit terminators
-- so we do that here
greyspacePreProcess :: Program -> Either LabelError GPP
greyspacePreProcess prog = coerce $ reLabel (concat $ linkBlock n $ blocks prog) 
  where n = freeLabel prog
