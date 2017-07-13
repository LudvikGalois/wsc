module Main where

import Language.Whitespace
import Language.Whitespace.Parser
import Language.Greyspace
import Language.Greyspace.Compiler
import Options.Applicative
import Data.Monoid
import Text.Trifecta.Parser (parseFromFile)
import qualified Text.Trifecta as T
import qualified LLVM as M
import qualified LLVM.Context as M
import qualified LLVM.Target as M
import qualified LLVM.PassManager as M
import System.IO
import System.Process
import System.Exit
import Data.Word

data WSCOptions = Compile { source :: FilePath, output :: FilePath
                          , stackSize :: Maybe Word64, heapSize :: Maybe Word64}
  | HumanReadable FilePath

makeCompile x = Compile {source = x, output = "a.out", stackSize = Nothing, heapSize = Nothing}

setOut :: WSCOptions -> FilePath -> WSCOptions
setOut x@Compile{} f = x {output = f}
setOut x _= x

setStackSize :: WSCOptions -> Maybe Integer -> WSCOptions
setStackSize x@Compile{} n = x {stackSize = fromIntegral <$> n}
setStackSize x _ = x

setHeapSize :: WSCOptions -> Maybe Integer -> WSCOptions
setHeapSize x@Compile{} n = x {heapSize = fromIntegral <$> n}
setHeapSize x _ = x


optionsParser :: Parser WSCOptions
optionsParser = setHeapSize <$>
  (setStackSize <$>
    (setOut <$>
      (flag makeCompile HumanReadable (long "hr" <> help "No compile, print human-readable source")
       <*> argument str (metavar "FILE")) <*> parseOutput) <*> parseStack) <*> parseHeap

parseOutput :: Parser String
parseOutput = strOption (long "output" <> short 'o' <> metavar "OUTPUT" <> value "a.out" <> help "Output file")

parseStack :: Parser (Maybe Integer)
parseStack = option auto (long "stack" <> short 's' <> metavar "STACK_SIZE" <> value Nothing <> help "Stack size")

parseHeap :: Parser (Maybe Integer)
parseHeap = option auto (long "heap" <> short 'm' <> metavar "HEAP_SIZE" <> value Nothing <> help "Heap size")

main :: IO ()
main = execWSC =<< execParser opts
  where opts = info (optionsParser <**> helper)
          (fullDesc <> progDesc "An optimising Whitespace compiler"
                    <> header "wsc - a whitespace compiler")

execWSC :: WSCOptions -> IO ()
execWSC Compile{source = f, output = o, stackSize = ss, heapSize = hs} = do
  res <- parseFromFile parseProgram f
  case res of
    Nothing -> exitWith (ExitFailure 1)
    Just prog -> case greyspacePreProcess prog of
      Left err -> hPrint stderr err >> exitWith (ExitFailure 1)
      Right prog' -> M.withContext $ \c -> do
        M.withModuleFromAST c (compile f ss hs (toGreyspace prog')) $ \m -> do
          M.withPassManager (M.defaultCuratedPassSetSpec {M.optLevel = Just 3, M.simplifyLibCalls = Just True}) $ \p -> do
            M.runPassManager p m
            M.withHostTargetMachine $ \h -> M.writeObjectToFile h (M.File (o ++ ".o")) m
        runLinker o
        return ()
execWSC (HumanReadable f) = do
  res <- parseFromFile parseProgram f
  case res of
    Nothing -> exitWith (ExitFailure 1)
    Just prog -> putStrLn (ppWhite prog)

runLinker outputName = do
  (Just inHandle, _, _, p) <- createProcess $ prog {std_in = CreatePipe, std_out = Inherit, std_err = Inherit}
  hPutStrLn inHandle rts
  waitForProcess p
  
  where prog = shell $ "cc -w -O3 " ++ outputName ++ ".o -o " ++ outputName ++ " -x c -"

-- Since we use a C-compiler so we can use libc, we might as well include a
-- small runtime in C instead of directly implementing them in LLVM
rts :: String
rts = unlines
  ["#include<stdio.h>"
  ,"#include<stdlib.h>"
  ,"void putnum(long int x){printf(\"%ld\",x);}"
  ,"long int getnum(){long int x;scanf(\"%ld\",&x);return x;}"]
