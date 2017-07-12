-- | A parser for the Whitespace programming language
module Language.Whitespace.Parser (parseProgram) where

import Data.List
import Control.Applicative
import Language.Whitespace
import Text.Trifecta hiding (space, tab, newline)
import qualified Text.Trifecta as T
import Numeric.Natural

comments :: CharParsing m => m Char
comments = noneOf " \t\n" <?> ""

-- We just want a space for this
space :: CharParsing m => m Char
space = (char ' ' <?> "space") <* skipMany comments

tab :: CharParsing m => m Char
tab = T.tab <* skipMany comments

newline :: CharParsing m => m Char
newline = T.newline <* skipMany comments

buildNatural :: [Bool] -> Natural
buildNatural = foldl' (\acc x -> if x then 1+2*acc else 2*acc) 0

-- Read a natural number. We allow a 0 digit number
parseNatural :: CharParsing m => m Natural
parseNatural = buildNatural <$> parseNatural'

-- Read a "number" as a list of bits
parseNatural' :: CharParsing m => m [Bool]
parseNatural' = (([] <$ newline)
  <|> (space *> ((False:) <$> parseNatural'))
  <|> (tab *> ((True:) <$> parseNatural'))) <?> "Bit string (spaces and tabs terminated by new-line)"

-- Read in an integer
parseNumber :: CharParsing m => m Integer
parseNumber = ((space *> (fromIntegral <$> parseNatural))
  <|> (tab *> (negate.fromIntegral <$> parseNatural))) <?> "Number (sign followed by bit string)"

parseStackOp :: CharParsing m => m StackOp
parseStackOp = (space *> (Push <$> parseNumber) <?> "Push (space)")
  <|> (newline *>
       ((space *> pure Dup <?> "Dup (space)") <|>
        (tab *> pure Swap <?> "Swap (tab)") <|>
        (newline *> pure Pop <?> "Pop (new-line)")))
  <|> (tab *>
       ((space *> (Peek <$> parseNumber) <?> "Peek (space)") <|>
        (tab *> space *> pure Shuffle <?> "Shuffle (tab space)") <|>
        (newline *> (Slide <$> parseNumber) <?> "Slide (new-line)")))

parseArithOp :: CharParsing m => m ArithOp
parseArithOp = 
  (space *>
    ((space *> pure Add <?> "Add (space)") <|>
     (tab *> pure Sub <?> "Sub (tab)") <|>
     (newline *> pure Mul <?> "Mul (new-line)")))
  <|> (tab *>
    ((space *> pure Div <?> "Div (space)") <|>
     (tab *> pure Mod <?> "Mod (tab)")))

parseHeap :: CharParsing m => m HeapOp
parseHeap = (space *> pure Store <?> "Store (space)")
  <|> (tab *> pure Load <?> "Load (tab)")

parseIO :: CharParsing m => m IOOp
parseIO =
  (space *>
    ((space *> pure PrintChar <?> "Print character (space)") <|>
     (tab *> pure PrintNum <?> "Print number (tab)")))
  <|> (tab *>
    ((space *> pure ReadChar <?> "Read character (space)") <|>
     (tab *> pure ReadNum <?> "Read number (tab)")))

-- This is special, because we moved labels out
-- to the top level of our AST, but we still want
-- our error messages to pretend we didn't so as
-- not to confuse users. Also since our labels are
-- numbers and not strings of bits, we just stick
-- a 1 in front of them to make them unique numbers
parseFlowControl :: CharParsing m => m Statement
parseFlowControl =
  (space *>
    ((space *> (Label <$> parseLabel) <?> "Label (space)") <|>
     (tab *> (ControlOp . Call <$> parseLabel) <?> "Call (tab)") <|>
     (newline *> (ControlOp . Jump <$> parseLabel) <?> "Jump (new-line)")))
  <|> (tab *>
    ((space *> (ControlOp . JumpZ <$> parseLabel) <?> "Jump-if-zero (space)") <|>
     (tab *> (ControlOp . JumpN <$> parseLabel) <?> "Jump-if-negative (tab)") <|>
      (newline *> pure (ControlOp Ret) <?> "Return (new-line)")))
  <|> (newline *> newline *> pure (ControlOp Exit) <?> "Exit (new-line new-line)")

parseLabel :: CharParsing m => m Natural
parseLabel = buildNatural . (True:) <$> parseNatural'

parseStatement :: CharParsing m => m Statement
parseStatement = (space *> (StackOp <$> parseStackOp) <?> "Stack operation (space)")
  <|> (tab *>
        ((space *> (ArithOp <$> parseArithOp) <?> "Arithmetic operation (space)") <|>
         (tab *> (HeapOp <$> parseHeap) <?> "Heap operation (tab)") <|>
         (newline *> (IOOp <$> parseIO) <?> "IO operation (new-line)")))
  <|> (newline *> parseFlowControl <?> "Flow control (new-line)")

-- | The parser itself
parseProgram :: CharParsing m => m Program
parseProgram = skipMany comments *> many parseStatement <* eof
