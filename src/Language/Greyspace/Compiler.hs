{-# LANGUAGE TupleSections #-}
{-# Language TemplateHaskell, OverloadedStrings #-}
-- | The compiler for they greyspace language.
-- since we limit the input method to via (valid)
-- whitespace, compilation should never be able to fail
module Language.Greyspace.Compiler (compile) where

import Language.Greyspace
import Control.Monad.State
import Data.ByteString.Short (ShortByteString)
import Data.Maybe
import Data.String
import Data.Word 
import Numeric.Natural
import Control.Lens

import LLVM.AST hiding (Terminator, Ret, Call, Add, Sub, Mul, Store, Load)
import LLVM.AST.CallingConvention 
import LLVM.AST.Type
import LLVM.AST.Linkage
import LLVM.AST.Visibility
import qualified LLVM.AST.Constant as C
import LLVM.AST.AddrSpace
import qualified LLVM.AST as L
import qualified LLVM.AST.IntegerPredicate as L
import qualified LLVM.Module as M
import qualified LLVM.Internal.Context as M
import qualified LLVM.PassManager as M

-- Only freshName changes, but I didn't feel like making a more complicated
-- Monad stack, so they're all stuck together
data GenState = GenState { _freshName :: Word, _stackName, _stackPtr
                         , _heapName, _argName :: Name} 
$(makeLenses ''GenState)
-- Since LLVM already required the lens library, I figured I'd be lazy
-- and use it, just for "use" and "+="

type CodeGen = State GenState

-- We had planned on optimising blocks
-- but it turns out all the planned optimisation
-- get done by LLVM for free. We might do them explicity
-- later, since if we change to using something like GMP
-- for arithmetic instead of LLVM instructions I don't think
-- the analysis will work as well

-- | Compile a greyspace program, with the given source name
-- and stack and heap sizes
compile :: String -> Maybe Word64 -> Maybe Word64 -> Program -> Module
compile filename stackS heapS p = evalState (codeGen filename stackS heapS p)
  (GenState (fresh+5) (UnName (fresh+1)) (UnName (fresh+2))
   (UnName (fresh+3)) (UnName (fresh+4)))
  where fresh = biggestLabel p
        biggestLabel = fromIntegral . label . last

-- Generate the code for a greyspace program
codeGen :: String -> Maybe Word64 -> Maybe Word64 -> Program -> CodeGen Module
codeGen filename stackS heapS prog = do
  blocks <- mapM genBlock prog
  stack <- use stackName
  ptr <- use stackPtr
  heap <- use heapName
  arg <- use argName
  return $ Module "whitespace_program" (fromString filename) Nothing Nothing
   [ GlobalDefinition (theStack stack)
   , GlobalDefinition (theHeap heap)
   , GlobalDefinition (theStackPtr ptr)
   , GlobalDefinition malloc
   , GlobalDefinition exitFun
   , GlobalDefinition putCharFun
   , GlobalDefinition putNumFun
   , GlobalDefinition getNumFun
   , GlobalDefinition getCharFun
   , GlobalDefinition (mainFun (fromMaybe stackSize stackS)
                       (fromMaybe heapSize heapS) stack heap)
   , GlobalDefinition (goFun arg (gostart arg callDests:blocks))
   ]
  where callDests = (UnName 0) : getCallDests prog

-- LLVM's indirect branch needs to know possible branch
-- points for reachability analysis
getCallDests :: Program -> [Name]
getCallDests [] = []
getCallDests (Block _ _ (Call d _):xs) = UnName (fromIntegral d) : getCallDests xs
getCallDests (_:xs) = getCallDests xs

-- We only use one address space
pointer :: Type -> Type
pointer typ = PointerType typ (AddrSpace 0)

-- A constant int (number of bits first)
cInt :: Word32 -> Integer -> Operand
cInt size val = ConstantOperand $ C.Int size val

-- Jump to the "function" that was invoked
gostart :: Name -> [Name] -> BasicBlock
gostart arg dests = BasicBlock (Name "gostart") []
  (Do $ IndirectBr (LocalReference (pointer i8) arg) dests [])

-- I don't know what half these args are, so let's just keep the default
-- values for most of them. TODO do this properly and without using
-- half the alphabet
theStack :: Name -> Global
theStack s = case globalVariableDefaults of
  GlobalVariable _ _ _ a b c d _ _ g h i j ->
    GlobalVariable s External Default a b c d
             (pointer i64) (AddrSpace 0) (Just $ C.Null $ (pointer i64)) h i j

theHeap :: Name -> Global
theHeap s = case globalVariableDefaults of
  GlobalVariable _ _ _ a b c d _ _ g h i j ->
    GlobalVariable s External Default a b c d
            (pointer i64) (AddrSpace 0) (Just $ C.Null $ (pointer i64)) h i j

theStackPtr :: Name -> Global
theStackPtr s = case globalVariableDefaults of
  GlobalVariable _ _ _ a b c d _ _ _ h i j ->
    GlobalVariable s External Default a b c d
                i64 (AddrSpace 0) (Just (C.Int 64 0)) h i j

-- The majority of our code ends up in this function
goFun :: Name -> [BasicBlock] -> Global
goFun s blocks = case functionDefaults of
  Function _ _ a b c _ _ (_,d) f g h i j k _ l -> 
    Function External Default a b c VoidType (Name "go")
                 ([Parameter (pointer i8) s []],d) f g h i j k blocks l

mallocCall :: CallableOperand
mallocCall = Right $ ConstantOperand $
  C.GlobalReference (FunctionType (pointer i64) [i64] False) (Name "malloc")

storeCommand :: Operand -> Operand -> Instruction
storeCommand target val = L.Store False target val Nothing 0 []

addInstr :: Operand -> Operand -> Instruction
addInstr op1 op2 = L.Add False False op1 op2 []

-- Boilerplate to initialise things
mainFun :: Word64 -> Word64 -> Name -> Name -> Global
mainFun stackS heapS stack heap = case functionDefaults of
  Function _ _ a b c _ _ (_,d) f g h i j k _ l -> 
    Function External Default a b c i32 (Name "main") ([],d) f g h i j k
    [BasicBlock (Name "mainStart")
     [ (Name "stackLoc" := L.Call Nothing C []
        mallocCall
         [(ConstantOperand (C.Int 64 (fromIntegral stackS)),[])] [] [])
     , (Name "heapLoc" := L.Call Nothing C []
        mallocCall
         [(ConstantOperand (C.Int 64 (fromIntegral heapS)),[])] [] [])
     , (Do $ storeCommand (globRef (pointer i64) stack)
        (LocalReference (pointer i64) (Name "stackLoc"))) 
     , (Do $ storeCommand (globRef (pointer i64) heap)
        (LocalReference (pointer i64) (Name "heapLoc")))
     , (Do $ L.Call Nothing C []
        (Right
          (ConstantOperand $ C.GlobalReference
           (FunctionType VoidType [pointer i8] False) (Name "go")))
         [(blockRef 0,[])] [] [])
     ]
      (Do $ L.Ret (Just (ConstantOperand $ C.Int 32 1)) [])] l

simpleExternal :: Type -> ShortByteString -> [Parameter] -> Global
simpleExternal retType name params = Function External Default Nothing C []
  retType (Name name) (params,False) [] Nothing Nothing 0
  Nothing Nothing [] Nothing

exitFun :: Global
exitFun = simpleExternal VoidType "exit" [Parameter i32 (Name "exitCode") []]

putCharFun :: Global
putCharFun = simpleExternal VoidType "putchar"
  [Parameter i32 (Name "outChar") []]

putNumFun :: Global
putNumFun = simpleExternal VoidType "putnum" [Parameter i64 (Name "outNum") []]

getNumFun :: Global
getNumFun = simpleExternal i64 "getnum" []

getCharFun :: Global
getCharFun = simpleExternal i32 "getchar" []

malloc :: Global
malloc = simpleExternal (pointer i64) "malloc"
  [Parameter i64 (Name "size") []]

genBlock :: Block -> CodeGen BasicBlock 
genBlock Block {label = l, stmts = stmts, term=term } = do
  let label = fromIntegral l
  code <- instrGen stmts
  (prol,closingOp) <- prologue term
  return $ BasicBlock (UnName label) (code++prol) (Do (makeTerm closingOp term))

instrGen :: [Statement] -> CodeGen [Named Instruction]
instrGen stmts = concat <$> mapM statementGen stmts

nextName :: CodeGen Name
nextName = do
  n <- use freshName
  freshName += 1
  return (UnName n)

names :: Int -> CodeGen [Name]
names n = sequence $ replicate n nextName

modifyPointer :: Operand -> CodeGen [Named Instruction]
modifyPointer op = do
  ptr <- use stackPtr
  [oldptr, newptr] <- names 2
  return [ oldptr := loadPtr ptr
         , newptr := addInstr (LocalReference i64 oldptr) op
         , Do (storePtr ptr (LocalReference i64 newptr))
         ]

arithGen op = do
  ptr <- use stackPtr
  stack <- use stackName
  decCode <- modifyPointer (cInt 64 (-1))
  [tix, ix, result] <- names 3
  (readCode, res) <- loadStack stack tix
  (readCode', res') <- loadStack stack ix
  outCode <- storeStack stack ix (LocalReference i64 result)
  let ixGetCode = [ tix := loadPtr ptr
                  , ix  := addInstr (LocalReference i64 tix) (cInt 64 (-1))]
  return $ concat [ decCode, ixGetCode, readCode, readCode'
                  , [result := (op res' res)], outCode]

-- A simple function call
simpleCall :: CallableOperand -> [Operand] -> Instruction
simpleCall fun params = L.Call Nothing C [] fun (map (,[]) params) [] []

-- Load the stack pointer
loadPtr :: Name -> Instruction
loadPtr ptr = L.Load False (globRef i64 ptr) Nothing 0 []

-- A global reference
globRef :: Type -> Name -> Operand
globRef typ name = ConstantOperand (C.GlobalReference typ name)

-- Default stack size
stackSize :: Word64
stackSize = 2^20

-- Default heap size
heapSize :: Word64
heapSize = 2^20

-- Load a value from the stack
-- TODO consider changing to taking operands
loadStack :: Name -> Name -> CodeGen ([Named Instruction], Name)
loadStack stack elem = do
  [preix, ix, res] <- names 3
  return ([ preix := L.Load False (globRef (pointer i64) stack) Nothing 0 []
          , ix := GetElementPtr True (LocalReference (pointer i64) preix) [LocalReference i64 elem] []
          , res := L.Load False (LocalReference i64 ix) Nothing 0 []
          ], res)

-- Load a value from the heap
loadHeap :: Name -> Name -> CodeGen ([Named Instruction], Name)
loadHeap = loadStack

-- Store a value somewhere in the stack
storeStack :: Name -> Name -> Operand -> CodeGen [Named Instruction]
storeStack stack elem val = do
  [preix, ix] <- names 2
  return [ preix := L.Load False (globRef (pointer i64) stack) Nothing 0 []
         , ix := GetElementPtr True (LocalReference (pointer i64) preix) [ LocalReference i64 elem] []
         , Do $ L.Store False (LocalReference i64 ix) val Nothing 0 []
         ]

-- Store a value somewhere in the heap
storeHeap :: Name -> Name -> Operand -> CodeGen [Named Instruction] 
storeHeap = storeStack

-- Store a new value in the stack pointer global variable
storePtr :: Name -> Operand -> Instruction
storePtr ptr val = L.Store False (globRef (pointer i64) ptr) val Nothing 0 []

-- Compile a statement to LLVM IR
statementGen :: Statement -> CodeGen [Named Instruction]
statementGen (Push n) = do
  ptr <- use stackPtr
  stack <- use stackName
  [oldptr] <- names 1
  storeInst <- storeStack stack oldptr (cInt 64 (fromIntegral n))
  updatePtr <- modifyPointer (cInt 64 1)
  return $[ oldptr := loadPtr ptr
          ] ++ updatePtr ++ storeInst

statementGen Pop = modifyPointer (cInt 64 (-1))

statementGen PrintChar = do
  ptr <- use stackPtr
  stack <- use stackName
  [ix, smaller] <- names 2
  decCode <- modifyPointer (cInt 64 (-1))
  (readCode, res) <- loadStack stack ix
  return $ decCode ++
         [ ix := loadPtr ptr] ++ readCode ++ 
         [ smaller := Trunc (LocalReference i64 res) i32 []
         , Do $ simpleCall
           (Right $ globRef (FunctionType VoidType [i32] False)
             (Name "putchar")) [LocalReference i32 smaller]]

statementGen PrintNum = do
  ptr <- use stackPtr
  stack <- use stackName
  [ix] <- names 1
  decCode <- modifyPointer (cInt 64 (-1))
  (readCode, res) <- loadStack stack ix
  return $ decCode ++
         [ ix := loadPtr ptr] ++ readCode ++ 
         [ Do $ simpleCall
           (Right $ globRef (FunctionType VoidType [i64] False) (Name "putnum"))
           [LocalReference i64 res]]

statementGen Add = arithGen
  (\x y -> addInstr (LocalReference i64 x) (LocalReference i64 y))
statementGen Sub = arithGen
  (\x y -> L.Sub False False (LocalReference i64 x) (LocalReference i64 y) [])
statementGen Mul = arithGen
  (\x y -> L.Mul False False (LocalReference i64 x) (LocalReference i64 y) [])
statementGen Mod = arithGen
  (\x y -> L.SRem (LocalReference i64 x) (LocalReference i64 y) [])
statementGen Div = arithGen
  (\x y -> L.SDiv False (LocalReference i64 x) (LocalReference i64 y) [])

statementGen Dup = do
  ptr <- use stackPtr
  stack <- use stackName
  [tix, ix] <- names 2
  (readCode, res) <- loadStack stack ix
  [oldptr, newptr] <- names 2
  incCode <- modifyPointer (cInt 64 1)
  storeInst <- storeStack stack oldptr (LocalReference i64 res)
  return $ [ tix := loadPtr ptr
           , ix := L.Add False False (LocalReference i64 tix) (cInt 64 (-1)) []
           ] ++ readCode ++
           [ oldptr := loadPtr ptr
           ] ++ incCode ++ storeInst

statementGen Swap = do
  ptr <- use stackPtr
  stack <- use stackName
  [tix, ix1, ix2] <- names 3
  (readCode1, res1) <- loadStack stack ix1
  (readCode2, res2) <- loadStack stack ix2
  store1 <- storeStack stack ix1 (LocalReference i64 res2)
  store2 <- storeStack stack ix2 (LocalReference i64 res1)
  return $ [ tix := loadPtr ptr
           , ix1 := addInstr (LocalReference i64 tix) (cInt 64 (-1))
           , ix2 := addInstr (LocalReference i64 tix) (cInt 64 (-2))
           ] ++ readCode1 ++ readCode2 ++ store1 ++ store2

-- TODO use modifyPointer in defining Slide
statementGen (Slide n) = do
  ptr <- use stackPtr
  stack <- use stackName
  [tix, ix1, ix2, top] <- names 4
  (readCode, val) <- loadStack stack top
  store <- storeStack stack ix2 (LocalReference i64 val)
  return $ [ tix := loadPtr ptr
           , ix1 := addInstr (LocalReference i64 tix) (cInt 64 (negate n))
           , ix2 := addInstr (LocalReference i64 ix1) (cInt 64 (-1))
           , top := addInstr (LocalReference i64 tix) (cInt 64 (-1))
           , Do $ storePtr ptr (LocalReference i64 ix1) 
           ] ++ readCode ++ store

statementGen (Peek n) = do
  ptr <- use stackPtr
  stack <- use stackName
  [tix, ix, ix2] <- names 3
  (readCode, val) <- loadStack stack ix
  store <- storeStack stack tix (LocalReference i64 val)
  return $ [ tix := loadPtr ptr
           , ix  := addInstr (LocalReference i64 tix) (cInt 64 (negate n))
           , ix2 := addInstr (LocalReference i64 tix) (cInt 64 1)
           , Do $ storePtr ptr (LocalReference i64 ix2)
           ] ++ readCode ++ store

statementGen Store = do
  ptr <- use stackPtr
  stack <- use stackName
  heap <- use heapName
  [tix, ix1, ix2] <- names 3
  (readCode1, res1) <- loadStack stack ix1
  (readCode2, res2) <- loadStack stack ix2
  store <- storeHeap heap res2 (LocalReference i64 res1)
  decCode <- modifyPointer (cInt 64 (-2))
  return $ [ tix := loadPtr ptr
           , ix1 := addInstr (LocalReference i64 tix) (cInt 64 (-1))
           , ix2 := addInstr (LocalReference i64 tix) (cInt 64 (-2))
           ] ++ readCode1 ++ readCode2 ++ store ++ decCode

statementGen Load = do
  ptr <- use stackPtr
  stack <- use stackName
  heap <- use heapName
  [tix, ix] <- names 2
  (loadCode, val) <- loadStack stack ix
  (heapLoad, heapVal) <- loadHeap heap val
  storeCode <- storeStack stack ix (LocalReference i64 heapVal)
  return $ [ tix := loadPtr ptr
           , ix  := addInstr (LocalReference i64 tix) (cInt 64 (-1))
           ] ++ loadCode ++ heapLoad ++ storeCode

statementGen ReadChar = do
  ptr <- use stackPtr
  stack <- use stackName
  heap <- use heapName
  [tix, ix, inChar, res] <- names 4
  (loadCode, val) <- loadStack stack ix
  storeCode <- storeHeap heap val (LocalReference i64 res)
  decCode <- modifyPointer (cInt 64 (-1))
  return $ [ tix := loadPtr ptr
           , ix := addInstr (LocalReference i64 tix) (cInt 64 (-1))
           , inChar := simpleCall (Right $ globRef (FunctionType i32 [] False)
                                   (Name "getchar")) []
           , res := L.SExt (LocalReference i32 inChar) i64 []
           ] ++ loadCode ++ storeCode ++ decCode

statementGen ReadNum = do
  ptr <- use stackPtr
  stack <- use stackName
  heap <- use heapName
  [tix, ix, inNum] <- names 3
  (loadCode, val) <- loadStack stack ix
  storeCode <- storeHeap heap val (LocalReference i64 inNum)
  decCode <- modifyPointer (cInt 64 (-1))
  return $ [ tix := loadPtr ptr
           , ix  := addInstr (LocalReference i64 tix) (cInt 64 (-1))
           , inNum := simpleCall (Right $ globRef (FunctionType i64 [] False)
                                  (Name "getnum")) []
           ] ++ loadCode ++ storeCode ++ decCode
statementGen x = error $ "Not yet implemented: " ++ (show x)

-- Take a reference to a block, and turn into address
blockRef :: Word -> Operand
blockRef n = ConstantOperand $ C.BlockAddress (Name "go") (UnName n)

-- Output a block's prologue based on terminator
-- For a conditional jump, we also return the operand
prologue :: Terminator -> CodeGen ([Named Instruction], Maybe Operand)
prologue (JumpZ _ _) = do
  ptr <- use stackPtr
  stack <- use stackName
  decCode <- modifyPointer (cInt 64 (-1))
  [ix, res] <- names 2
  (readCode, val) <- loadStack stack ix
  return $
    (decCode ++
     [ ix := loadPtr ptr] ++ readCode ++
     [ res := ICmp L.EQ (LocalReference i64 val) (cInt 64 0) []]
    , Just (LocalReference i64 res))
prologue (JumpN _ _) = do
  ptr <- use stackPtr
  stack <- use stackName
  decCode <- modifyPointer (cInt 64 (-1))
  [ix, res] <- names 2
  (readCode, val) <- loadStack stack ix
  return $
    (decCode ++
     [ ix := loadPtr ptr] ++ readCode ++
     [ res := ICmp L.SLT (LocalReference i64 val) (cInt 64 0) []]
    , Just (LocalReference i64 res))
prologue (Call l _) = return
  ([Do $ simpleCall
        (Right (globRef (FunctionType VoidType [pointer i8] False) (Name "go")))
        [blockRef n]]
  , Nothing)
  where n = fromIntegral l

prologue Exit = return 
  ([Do $ simpleCall
        (Right (globRef (FunctionType VoidType [i32] False) (Name "exit")))
        [cInt 32 0]]
  , Nothing)

-- Other commands don't need and prologue generated
prologue _ = return ([], Nothing)
  
-- Convert a greyspace terminator into an LLVM terminator
makeTerm :: Maybe Operand -> Terminator -> L.Terminator
makeTerm _ Ret = L.Ret Nothing []
makeTerm _ Exit = Unreachable []
makeTerm _ (Jump l) = Br (toJump l) []
makeTerm (Just n) (JumpZ l1 l2) = CondBr n (toJump l1) (toJump l2) []
makeTerm (Just n) (JumpN l1 l2) = CondBr n (toJump l1) (toJump l2) []
makeTerm _ (Call _ l) = Br (toJump l) []

-- Convert a greyspace label into an LLVM name
toJump :: Natural -> Name
toJump = UnName . fromIntegral

-- Some things for testing
testStr :: Program -> IO String
testStr x = (read.show) <$> -- dodgy bytestring to string
  (M.withContext $ \c ->
      M.withModuleFromAST c
      (compile "test" Nothing Nothing x) M.moduleLLVMAssembly)

optTestStr :: Program -> IO String
optTestStr x = (read.show) <$>
  (M.withContext $ \c ->
      M.withModuleFromAST c
      (compile "test" Nothing Nothing x)
      (\m -> M.withPassManager M.defaultCuratedPassSetSpec
        (\p -> M.runPassManager p m >> M.moduleLLVMAssembly m)))
