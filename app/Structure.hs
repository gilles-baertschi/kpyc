module Structure (
    Type (..),
    Eval (..),
    VariableDef (..),
    FunctionDef (..),
    FunctionCall (..),
    Instruction (..),
    Context (..),
    Program (..),
    builtInDefs,
    Offset (..),
    BuiltInDef (..),
    Superposition (..),
    defaultProgram,
    Backup
    ) where

import qualified RawStructure as Raw

data Offset = Offset Int | OffsetName Int
    deriving (Eq, Ord)

instance Show Offset where
    show (Offset offset)
        | actualOffset > 0 = "[rbp+" ++ show actualOffset ++ "]"
        | actualOffset < 0 = "[rbp" ++ show actualOffset ++ "]"
        | otherwise = "[rbp]"
            where actualOffset = offset * (-8)
    show (OffsetName index) = "[var" ++ show index ++ "]"

data Type = PyBool | PyInt | PyFloat | PyString | PyArray Type  | PyGeneric
    deriving (Eq)

instance Show Type where
    show PyBool = "Bool"
    show PyInt = "Int"
    show PyFloat = "Float"
    show PyString = "String"
    show PyGeneric = "T"
    show (PyArray innerType) = "[" ++ show innerType ++ "]"

data Superposition = Superposition [Type] | SuperpositionReference Int | AnyType | SuperpositionArray Superposition | Generic
    deriving (Eq, Show)

data Eval = BoolLiteral Bool | IntLiteral Int | FloatLiteral Int | StringLiteral Int | ArrayLiteral [Eval] | ArrayElement Eval Eval | VariableEval Offset | FunctionEval FunctionCall
    deriving (Eq, Show)

type Backup = (Superposition, [Context], Offset, Bool)

data VariableDef = VariableDef { 
    getType :: Superposition, 
    getLineNumber :: Int,
    getVariableName :: String, 
    getVariableOffset:: Offset }
    deriving (Eq, Show)

data FunctionDef = FunctionDef { 
    getSignature :: [Superposition], 
    getReturnType :: Superposition, 
    getFunctionName :: String, 
    getContext :: Context } | 
    BuiltInDef { getSignature :: [Superposition], 
    getReturnType :: Superposition, 
    getFunctionName :: String, 
    getBuiltInDef :: BuiltInDef }
    deriving (Eq, Show)

data FunctionCall = FunctionCall Int [Eval]
    deriving (Eq, Show)

data Instruction = Execution FunctionCall | Assignment Offset Eval | Return Eval | If Int [Eval] [Context] | For Int Eval Context | While Int Eval Context
    deriving (Eq, Show)

data Context = Context { 
    getVariableDefs :: [VariableDef], 
    getInstructions :: [Instruction] }
    deriving (Eq, Show)

data Program = Program { 
    getFunctionDefs :: [FunctionDef], 
    getGlobalContext :: Context, 
    getStringLiterals :: [String], 
    getFloatLiterals :: [Double], 
    getSuperpositions :: [Superposition],
    getCurrentContexts :: [Context], 
    getRawFunctionDefs :: [Raw.FunctionDef],
    getCurrentLine :: Int,
    getCurrentIf :: Int,
    getCurrentWhile :: Int,
    getCurrentFor :: Int,
    getCurrentGlobal :: Int,
    getCurrentOffset :: Offset,
    getCurrentReturnType :: Superposition,
    getIsMainContext :: Bool,
    getGeneric :: Superposition }
    deriving (Eq, Show)

data BuiltInDef = 
    Input | PrintInput | PrintString | PrintBool | PrintInt | PrintFloat | 
    BoolToString | IntToString | FloatToString | 
    Range0 | Range | ToInt | ConcatString | Len | ConcatList |
    EqualsBool | EqualsString | EqualsInt | EqualsFloat |
    AddInt | AddFloat | SubtractInt | SubstractFloat | NegateInt | NegateFloat | MultiplyInt | MultiplyFloat | DivideInt | DivideFloat |
    LessInt | LessFloat | GreaterInt | GreaterFloat | LessEqualsInt | LessEqualsFloat | GreaterEqualsInt | GreaterEqualsFloat |
    AndBool | OrBool | NotBool
    deriving (Eq, Show)

defaultProgram :: Program
defaultProgram = Program { 
    getFunctionDefs = [], 
    getGlobalContext = Context [] [], 
    getStringLiterals = [], 
    getFloatLiterals = [], 
    getSuperpositions = [],
    getCurrentContexts = [Context [] []],
    getRawFunctionDefs = [],
    getCurrentLine = 0,
    getCurrentIf = 0,
    getCurrentWhile = 0,
    getCurrentFor = 0,
    getCurrentGlobal = 0,
    getCurrentOffset = Offset 1,
    getCurrentReturnType = Superposition [],
    getIsMainContext = True,
    getGeneric = Superposition [] }

builtInDefs :: [(BuiltInDef, String, [Type], Maybe Type)]
builtInDefs = [
    (PrintString, "print", [PyString], Nothing),
    (PrintBool, "print", [PyBool], Nothing),
    (PrintInt, "print", [PyInt], Nothing),
    (PrintFloat, "print", [PyFloat], Nothing),
    (BoolToString, "str", [PyBool], Just PyString),
    (IntToString, "str", [PyInt], Just PyString),
    (FloatToString, "str", [PyFloat], Just PyString),
    (Input, "input", [], Just PyString),
    (PrintInput, "input", [PyString], Just PyString),
    (Len, "len", [PyString], Just PyInt),
    (Range0, "range", [PyInt], Just $ PyArray PyInt),
    (Range, "range", [PyInt, PyInt], Just $ PyArray PyInt),
    (ToInt, "int", [PyString], Just PyInt),
    (EqualsInt, "==", [PyInt, PyInt], Just PyBool),
    (EqualsString, "==", [PyString, PyString], Just PyBool),
    (EqualsBool, "==", [PyBool, PyBool], Just PyBool),
    (EqualsFloat, "==", [PyFloat, PyFloat], Just PyBool),
    (AddInt, "+", [PyInt, PyInt], Just PyInt),
    (AddFloat, "+", [PyFloat, PyFloat], Just PyFloat),
    (ConcatString, "+", [PyString, PyString], Just PyString),
    (ConcatList, "+", [PyArray PyGeneric, PyArray PyGeneric], Just $ PyArray PyGeneric),
    (SubtractInt, "-", [PyInt, PyInt], Just PyInt),
    (SubstractFloat, "-", [PyFloat, PyFloat], Just PyFloat),
    (NegateInt, "-", [PyInt], Just PyInt),
    (NegateFloat, "-", [PyFloat], Just PyFloat),
    (MultiplyInt, "*", [PyInt, PyInt], Just PyInt),
    (MultiplyFloat, "*", [PyFloat, PyFloat], Just PyFloat),
    (DivideInt, "/", [PyInt, PyInt], Just PyInt),
    (DivideFloat, "/", [PyFloat, PyFloat], Just PyFloat),
    (LessInt, "<", [PyInt, PyInt], Just PyBool),
    (LessFloat, "<", [PyFloat, PyFloat], Just PyBool),
    (GreaterInt, ">", [PyInt, PyInt], Just PyBool),
    (GreaterFloat, ">", [PyFloat, PyFloat], Just PyBool),
    (LessEqualsInt, "<=", [PyInt, PyInt], Just PyBool),
    (LessEqualsFloat, "<=", [PyFloat, PyFloat], Just PyBool),
    (GreaterEqualsInt, ">=", [PyInt, PyInt], Just PyBool),
    (GreaterEqualsFloat, ">=", [PyFloat, PyFloat], Just PyBool),
    (AndBool, "and", [PyBool, PyBool], Just PyBool),
    (OrBool, "or", [PyBool, PyBool], Just PyBool),
    (NotBool, "not", [PyBool], Just PyBool)]