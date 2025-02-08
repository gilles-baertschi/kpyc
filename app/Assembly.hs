{-# LANGUAGE TemplateHaskell #-}

module Assembly (execBuilder) where

import Structure
import Control.Monad.Reader
import Control.Monad.Writer
import Data.FileEmbed (embedStringFile)
import qualified Data.Set as Set

definitions :: String
definitions = $(embedStringFile "Definitions.asm")

type Builder = ReaderT Program (Writer String)

execBuilder :: Program -> String
execBuilder program = execWriter (runReaderT writeProgram program)

writeLiterals :: Builder ()
writeLiterals = do
    stringLiterals <- asks getStringLiterals
    floatLiterals <- asks getFloatLiterals
    tell "section .data\nnewline db 10, 0\ntrue db \"True\", 0\nfalse db \"False\", 0\nsign_mask dq 0x8000000000000000\nabs_mask dq 0x7FFFFFFFFFFFFFFF\n"
    tell $ unlines $ zipWith writeStringLine [0..] stringLiterals
    tell $ unlines $ zipWith writeFloatLine [0..] floatLiterals
        where writeStringLine index literal = getStringLiteralName index ++ " db \"" ++ literal ++ "\", 0"
              writeFloatLine index literal = getFloatLiteralName index ++ " dq " ++ show literal

writeReserves :: Builder ()
writeReserves = do
    offsets <- asks (getUniqueOffsets . getGlobalContext)
    tell "section .bss\n"
    tell $ concatMap writeLine offsets
        where
            writeLine (OffsetName index) = "var" ++ show index ++ " resq 1\n"
            writeLine _ = ""

getUniqueOffsets :: Context -> [Offset]
getUniqueOffsets = (Set.toList . Set.fromList) . getOffsetsInContext
    where
        getOffsetsInContext context = (getVariableOffset <$> getVariableDefs context) ++ concatMap getOffsetsInInstruction (getInstructions context)
        getOffsetsInInstruction (If _ _ contexts) = concatMap getOffsetsInContext contexts
        getOffsetsInInstruction (While _ _ context) = getOffsetsInContext context
        getOffsetsInInstruction (For _ _ context) = getOffsetsInContext context
        getOffsetsInInstruction _ = []

getStringLiteralName :: Int -> String
getStringLiteralName index = "str" ++ show index

getFloatLiteralName :: Int -> String
getFloatLiteralName index = "float" ++ show index

getFunctionNameFromIndex :: Int -> String
getFunctionNameFromIndex index = "fun" ++ show index

writeProgram :: Builder ()
writeProgram = do
    writeLiterals
    writeReserves
    tell "\nsection .text\nglobal _start\n"
    globalContext <- asks getGlobalContext
    writeContext False "_start" globalContext
    tell "mov rax,60\nxor rdi,rdi\nsyscall\n"
    contexts <- asks (map getContext . filter isNotBuiltInDef . getFunctionDefs)
    zipWithM_ (writeContext True . getFunctionNameFromIndex) [0..] contexts
    tell definitions
        where
            isNotBuiltInDef functionDef = case functionDef of
                BuiltInDef {} -> False
                FunctionDef {} -> True

writeContext :: Bool -> String -> Context -> Builder ()
writeContext ret name (Context variableDefs instructions) = do
    tell $ name ++ ":\n"
    tell "push rbp\nmov rbp, rsp\n"
    let variableLength = maximum $ 0 : (getOffset <$> getUniqueOffsets (Context variableDefs instructions))
    tell $ "sub rsp," ++ show (variableLength * 8) ++ "\n"
    mapM_ writeInstruction instructions
    tell "mov rsp, rbp\npop rbp\n"
    when ret $ tell "ret\n"
        where
            getOffset (Offset offset) = offset
            getOffset _ = 0;

writeInstruction :: Instruction -> Builder ()
writeInstruction (Assignment offset eval) = do
    writeEval eval
    let destination = show offset
    tell $ "mov " ++ destination  ++ ",rax\n"
writeInstruction (Execution functionCall) = writeFunctionCall functionCall
writeInstruction (Return eval) = do
    writeEval eval
    tell "mov rsp, rbp\npop rbp\nret\n"
writeInstruction (If index conditions contexts) = do
    let hasElse = length conditions /= length contexts
    zipWithM_ (\subIndex condition -> do
        writeEval condition
        tell $ "test rax,rax\njnz " ++ lableIf subIndex ++ "\n"
        ) ([0..] :: [Int]) conditions
    when hasElse $ tell $ "jmp " ++ lableElse ++ "\n"
    tell $ "jmp " ++ lableDone ++ "\n"
    zipWithM_ (\subIndex context -> do
        tell $ lableIf subIndex ++ ":\n"
        mapM_ writeInstruction $ getInstructions context
        tell $ "jmp " ++ lableDone ++ "\n"
        ) ([0..] :: [Int]) contexts
    when hasElse (do
        tell $ lableElse ++ ":\n"
        mapM_ writeInstruction $ getInstructions $ last contexts)
    tell $ lableDone ++ ":\n"
        where
            lableIf subIndex = ".if_" ++ show index ++ "_" ++ show subIndex
            lableElse = ".else_" ++ show index
            lableDone = ".done_if_" ++ show index
writeInstruction (While index condition context) = do
    tell $ lableLoop ++ ":\n"
    writeEval condition
    tell $ "test rax,rax\njz " ++ lableDone ++ "\n"
    mapM_ writeInstruction $ getInstructions context
    tell $ "jmp " ++ lableLoop ++ "\n"
    tell $ lableDone ++ ":\n"
        where
            lableLoop = ".loop_while_" ++ show index
            lableDone = ".done_while_" ++ show index
writeInstruction (For index array context) = do
    writeInstruction $ Assignment counterOffset $ IntLiteral 0
    writeInstruction $ Assignment arrayOffset array
    tell $ "push 0\n" ++ lableLoop ++ ":\n"
    tell $ "mov rax," ++ show counterOffset ++ "\npush rax\nmov rdi," ++ show arrayOffset ++ "\nmov rcx,[rdi]\npop rax\ncmp rax,rcx\njge " ++ lableDone ++ "\n"
    tell $ "inc qword " ++ show counterOffset ++ "\n"
    tell $ "inc rax\nshl rax,3\nadd rdi,rax\nmov rax,[rdi]\nmov " ++ show elementOffset ++ ",rax\n"
    mapM_ writeInstruction $ getInstructions context
    tell $ "jmp " ++ lableLoop ++ "\n"
    tell $ lableDone ++ ":\n"
        where
            lableLoop = ".loop_for_" ++ show index
            lableDone = ".done_for_" ++ show index
            (counterOffset, arrayOffset, elementOffset) = case getVariableOffset <$> getVariableDefs context of
                (a : b : c : _) -> (a, b, c)
                _ -> undefined

writeBuiltInDef :: BuiltInDef -> String
writeBuiltInDef builtInDef = case builtInDef of
    PrintString -> "call print\npush newline\ncall print\npop rax\n"
    PrintBool -> "call bool_to_string\npush rax\ncall print\npush newline\ncall print\nsub rsp,8\n\npop rax\n"
    PrintInt -> "call int_to_string\npush rax\ncall print\npush newline\ncall print\nsub rsp,8\npop rax\n"
    PrintFloat -> "call float_to_string\npush rax\ncall print\npush newline\ncall print\nsub rsp,8\npop rax\n"
    
    BoolToString -> "call bool_to_string\n"
    IntToString -> "call int_to_string\n"
    FloatToString -> "call float_to_string\n"
    
    Input -> "call input\n"
    PrintInput -> "call print\ncall input\n"
    ToInt -> "call string_to_int\n"
    Range0 -> "call range0\n"
    Range -> "call range\n"
    ConcatString -> "call concat\n"
    ConcatList -> "call concat_list\n" 
    Len -> "mov rdi,[rsp]\ncall buflen\n"
    
    EqualsString -> "call equals_string\n"

    EqualsBool -> "mov rax,[rsp+8]\nand rax,1\nmov rcx,[rsp]\nand rcx,1\ncmp rax,rcx\nsete al\nmovzx rax,al\n"
    AndBool -> "mov rax,[rsp+8]\nand rax,[rsp]\n"
    OrBool -> "mov rax,[rsp+8]\nor rax,[rsp]\n"
    NotBool -> "mov rax,[rsp]\nxor rax,1\n"
    
    EqualsInt -> "mov rax,[rsp+8]\ncmp rax,[rsp]\nsete al\nmovzx rax,al\n"
    AddInt -> "mov rax,[rsp+8]\nmov rcx,[rsp]\nadd rax,rcx\n"
    SubtractInt -> "mov rax,[rsp+8]\nmov rcx,[rsp]\nsub rax,rcx\n"
    NegateInt -> "mov rax,[rsp]\nneg rax\n"
    MultiplyInt -> "mov rax,[rsp+8]\nimul rax,[rsp]\n"
    DivideInt -> "xor rdx,rdx\nmov rax,[rsp+8]\nidiv qword [rsp]\n"
    LessInt -> "mov rax,[rsp+8]\ncmp rax,[rsp]\nsetl al\nmovzx rax,al\n"
    GreaterInt -> "mov rax,[rsp+8]\ncmp rax,[rsp]\nsetg al\nmovzx rax,al\n"
    LessEqualsInt -> "mov rax,[rsp+8]\ncmp rax,[rsp]\nsetle al\nmovzx rax,al\n"
    GreaterEqualsInt -> "mov rax,[rsp+8]\ncmp rax,[rsp]\nsetge al\nmovzx rax,al\n"
    
    EqualsFloat -> "movsd xmm0,[rsp]\nmovsd xmm1,[rsp+8]\nucomisd xmm0,xmm1\nsete al\nmovzx rax,al\n"
    AddFloat -> "movsd xmm0, [rsp+8]\nmovsd xmm1,[rsp]\naddsd xmm0,xmm1\nmovq rax,xmm0\n"
    SubstractFloat -> "movsd xmm0,[rsp+8]\nmovsd xmm1,[rsp]\nsubsd xmm0,xmm1\nmovq rax,xmm0\n"
    NegateFloat -> "movsd xmm0,[rsp]\nmovsd xmm1, qword [sign_mask]\nxorpd xmm0,xmm1\nmovq rax,xmm0\n"
    MultiplyFloat -> "movsd xmm0,[rsp+8]\nmovsd xmm1,[rsp]\nmulsd xmm0,xmm1\nmovq rax,xmm0\n"
    DivideFloat -> "movsd xmm0,[rsp+8]\nmovsd xmm1,[rsp]\ndivsd xmm0,xmm1\nmovq rax,xmm0\n"
    LessFloat -> "movsd xmm0,[rsp]\nmovsd xmm1,[rsp+8]\nucomisd xmm0,xmm1\nsetg al\nmovzx rax,al\n"
    GreaterFloat -> "movsd xmm0,[rsp]\nmovsd xmm1,[rsp+8]\nucomisd xmm0,xmm1\nsetl al\nmovzx rax,al\n"
    LessEqualsFloat -> "movsd xmm0,[rsp]\nmovsd xmm1,[rsp+8]\nucomisd xmm0,xmm1\nsetge al\nmovzx rax,al\n"
    GreaterEqualsFloat -> "movsd xmm0,[rsp]\nmovsd xmm1,[rsp+8]\nucomisd xmm0,xmm1\nsetle al\nmovzx rax,al\n"

writeEval :: Eval -> Builder ()
writeEval (BoolLiteral value) = tell $ "mov rax," ++ (if value then "1" else "0") ++ "\n"
writeEval (IntLiteral value) = tell $ "mov rax," ++ show value ++ "\n"
writeEval (FloatLiteral index) = tell $ "mov rax, qword [" ++ getFloatLiteralName index ++ "]\n"
writeEval (StringLiteral index) = tell $ "mov rax," ++ getStringLiteralName index ++ "\n"
writeEval (FunctionEval functionCall) = writeFunctionCall functionCall
writeEval (VariableEval source) = tell $ "mov rax" ++ "," ++ show source ++ "\n"
writeEval (ArrayLiteral evals) = do
    tell $ "mov rdi," ++ show ((length evals + 1) * 8) ++ "\ncall alloc\npush rax\n"
    tell $ "mov qword [rax]," ++ show (length evals) ++ "\n"
    zipWithM_ (\index eval -> do
        writeEval eval
        tell $ "mov rcx,[rsp]\nmov [rcx+" ++ show (index * 8) ++ "],rax\n"
        ) ([1..] :: [Int]) evals
    tell "pop rax\n"
writeEval (ArrayElement arrayEval indexEval) = do 
    writeEval arrayEval
    tell "push rax\n"
    writeEval indexEval
    tell "inc rax\nshl rax,3\nadd rax,[rsp]\nmov rax,[rax]\nadd rsp,8\n"

writeFunctionCall :: FunctionCall -> Builder ()
writeFunctionCall (FunctionCall index arguments) = do
    mapM_ (\a -> do writeEval a; tell "push rax\n") arguments
    functionDefs <- asks getFunctionDefs
    case functionDefs !! index of
        BuiltInDef _ _ _ builtInDef -> tell $ writeBuiltInDef builtInDef
        FunctionDef {} -> tell $ "call " ++ getFunctionNameFromIndex (index - length builtInDefs) ++ "\n"
    tell $ "add rsp, " ++ show (length arguments * 8) ++ "\n"
