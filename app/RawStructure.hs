module RawStructure
(Eval (..), FunctionCall (..), Instruction (..), FunctionDef (..), Program (..), getSourcePos, isLiteral) where

import Text.Parsec

data Eval = BoolLiteral SourcePos Bool | IntLiteral SourcePos Int | FloatLiteral SourcePos Double | StringLiteral SourcePos String | ArrayLiteral SourcePos [Eval] | VariableEval SourcePos String | ArrayElement Eval Eval | FunctionEval  FunctionCall
    deriving (Show, Eq)
data FunctionCall = FunctionCall SourcePos String [Eval]
    deriving (Show, Eq)
data Instruction = Execution FunctionCall | Assignment String Eval | Return SourcePos Eval | If [Eval] [[Instruction]] | For String Eval [Instruction] | While Eval [Instruction]
    deriving (Show, Eq)
data FunctionDef = FunctionDef { getFunctionName :: String, getArgumentNames :: [String], getInstructions :: [Instruction] }
    deriving (Show, Eq)
data Program = Program [FunctionDef] [Instruction]
    deriving (Show, Eq)

isLiteral :: Eval -> Bool
isLiteral eval = case eval of
    BoolLiteral _ _ -> True
    IntLiteral _ _ -> True
    FloatLiteral _ _ -> True
    StringLiteral _ _ -> True
    _ -> False

getSourcePos :: Eval -> SourcePos
getSourcePos eval = case eval of
    BoolLiteral pos _ -> pos
    IntLiteral pos _ -> pos
    FloatLiteral pos _ -> pos
    StringLiteral pos _ -> pos
    ArrayLiteral pos _ -> pos
    VariableEval pos _ -> pos
    FunctionEval (FunctionCall pos _ _) -> pos
    ArrayElement arrayEval _ -> getSourcePos arrayEval