module Parsers where

import Structure
import qualified RawStructure as Raw
import qualified RawParsers as Raw
import Control.Monad.State
import Data.Maybe
import Data.List
import Data.Either (isRight)
import Text.Parsec.String
import Text.Parsec

type ParserWithState s a = StateT s Parser a

applyParserWithState :: ParserWithState s a -> s -> String -> Either ParseError s
applyParserWithState p initialState = runParser (execStateT p initialState) () ""

programP :: ParserWithState Program ()
programP = do
    (Raw.Program functionDefs instructions) <- lift $ try $ lookAhead Raw.programP
    createBuiltInDefs
    modify (\program -> program { getRawFunctionDefs = functionDefs })
    mapM_ consumeMainInstruction instructions

createBuiltInDefs :: ParserWithState Program ()
createBuiltInDefs = do
    functionDefs <- mapM (\(builtInDef, name, signature, returnType) -> do
        signatureSuperpositions <- mapM addSuperposition (convert <$> signature)
        returnSuperposition <- addSuperposition $ maybe (Superposition []) convert returnType
        return $ BuiltInDef signatureSuperpositions returnSuperposition name builtInDef) builtInDefs
    modify (\program -> program { getFunctionDefs = functionDefs })
        where
            convert (PyArray innerType) = SuperpositionArray $ convert innerType
            convert PyGeneric = Generic
            convert pyType = Superposition [pyType]

consumeMainInstruction :: Raw.Instruction -> ParserWithState Program ()
consumeMainInstruction (Raw.Return pos _) = failAtPosition pos "You can only return from functions."
consumeMainInstruction rawInstruction = consumeInstruction rawInstruction >> writeMainContext

consumeInstruction :: Raw.Instruction -> ParserWithState Program ()
consumeInstruction (Raw.Assignment name rawEval) = do
    (Offset offsetBackup) <- gets getCurrentOffset
    (superposition, eval) <- consumeEval rawEval
    variableDefsWithName <- getRelevantVariableDefsWithName name
    let maybeVariableType = getType <$> listToMaybe (reverse variableDefsWithName)
    alreadyDefined <- maybe (return False) (fmap isRight . tryMergeSuperposition superposition) maybeVariableType
    unless alreadyDefined $ addVariableDef superposition name
    incrementLine
    offset <- getVariableOffset . last <$> getRelevantVariableDefsWithName name
    addInstruction $ Assignment offset eval
    modify (\program -> program { getCurrentOffset = if alreadyDefined then Offset offsetBackup else Offset $ offsetBackup + 1 })
consumeInstruction (Raw.Execution rawFunctionCall) = do
    offsetBackup <- gets getCurrentOffset
    (_, functionCall) <- consumeFunctionCall rawFunctionCall
    addInstruction $ Execution functionCall
    modify (\program -> program { getCurrentOffset = offsetBackup })
consumeInstruction (Raw.Return _ rawEval) = do
    currentReturnType <- gets getCurrentReturnType
    (newReturnType, eval) <- assertType currentReturnType rawEval
    modify (\program -> program { getCurrentReturnType = newReturnType })
    addInstruction $ Return eval
    incrementLine
consumeInstruction (Raw.If rawConditions rawInstructions) = do
    (_, conditions) <- mapAndUnzipM (assertType (Superposition [PyBool])) rawConditions
    contexts <- mapM (\instructions -> do
        backup <- enterContext False []
        mapM_ consumeInstruction instructions
        leaveContext backup) rawInstructions
    index <- gets getCurrentIf
    modify (\program -> program { getCurrentIf = index + 1 })
    addInstruction $ If index conditions contexts
consumeInstruction (Raw.While rawCondition rawInstructions) = do
    (_, condition) <- assertType (Superposition [PyBool]) rawCondition
    context <- do
        backup <- enterContext False []
        mapM_ consumeInstruction rawInstructions
        leaveContext backup
    index <- gets getCurrentWhile
    modify (\program -> program { getCurrentWhile = index + 1 })
    addInstruction $ While index condition context
consumeInstruction (Raw.For name rawArrayEval rawInstructions) = do
    (outerSuperposition, _) <- createEmptyArray
    (superpositionReference, arrayEval) <- assertType outerSuperposition rawArrayEval
    superposition <- dereferenceSuperposition superpositionReference
    case superposition of
        SuperpositionArray innerSuperposition -> do
            context <- do
                backup <- enterContext False [(Superposition [PyInt], ""), (superposition, ""), (innerSuperposition, name)]
                mapM_ consumeInstruction rawInstructions
                leaveContext backup
            index <- gets getCurrentFor
            modify (\program -> program { getCurrentFor = index + 1 })
            addInstruction $ For index arrayEval context
        _ -> undefined

consumeEval :: Raw.Eval -> ParserWithState Program (Superposition, Eval)
consumeEval (Raw.BoolLiteral _ value) = return (Superposition [PyBool], BoolLiteral value)
consumeEval (Raw.IntLiteral _ value) = return (Superposition [PyInt], IntLiteral value)
consumeEval (Raw.FloatLiteral _ value) = do
    floatLiterals <- gets getFloatLiterals
    let maybeIndex = elemIndex value floatLiterals
    floatIndex <- maybe (addFloatLiteral value) return maybeIndex
    return (Superposition [PyFloat], FloatLiteral floatIndex)
consumeEval (Raw.StringLiteral _ value) = do
    stringLiterals <- gets getStringLiterals
    let maybeIndex = elemIndex value stringLiterals
    stringIndex <- maybe (addStringLiteral value) return maybeIndex
    return (Superposition [PyString], StringLiteral stringIndex)
consumeEval (Raw.ArrayLiteral pos rawEvals) = do
    if null rawEvals
        then do
            (outerSuperposition, _) <- createEmptyArray
            return (outerSuperposition, ArrayLiteral [])
        else do
            innerSuperposition <- addSuperposition AnyType
            (evals, finalSuperposition) <- foldM (\(evals, currentSuperposition) rawEval -> do
                (newSuperposition, eval) <- consumeEval rawEval
                resultingSuperposition <- mergeSuperposition pos currentSuperposition newSuperposition
                return (evals ++ [eval], resultingSuperposition)) ([], innerSuperposition) rawEvals
            finalArraySuperposition <- addSuperposition $ SuperpositionArray finalSuperposition
            return (finalArraySuperposition, ArrayLiteral evals)
consumeEval (Raw.VariableEval pos name) = do
    variableDef <- getVariable pos name
    return (getType variableDef, VariableEval (getVariableOffset variableDef))
consumeEval (Raw.FunctionEval rawFunctionCall) = do
    (returSuperposition, functionCall) <- consumeFunctionCall rawFunctionCall
    return (returSuperposition, FunctionEval functionCall)
consumeEval (Raw.ArrayElement rawArrayEval rawIndexEval) = do
    (_, indexEval) <- assertType (Superposition [PyInt]) rawIndexEval
    (outerSuperposition, _) <- createEmptyArray
    (arraySuperposition, arrayEval) <- assertType outerSuperposition rawArrayEval
    (SuperpositionArray innerSuperposition) <- dereferenceSuperposition arraySuperposition
    return (innerSuperposition, ArrayElement arrayEval indexEval)

createEmptyArray :: ParserWithState Program (Superposition, Superposition)
createEmptyArray = do
    innerSuperposition <- addSuperposition AnyType
    outerSuperposition <- addSuperposition $ SuperpositionArray innerSuperposition
    return (outerSuperposition, innerSuperposition)

consumeFunctionCall :: Raw.FunctionCall -> ParserWithState Program (Superposition, FunctionCall)
consumeFunctionCall (Raw.FunctionCall pos name rawEvals) = do
    functionDefs <- gets getFunctionDefs
    let functionDefsWithName = filter (\functionDef -> getFunctionName functionDef == name) functionDefs
    if null functionDefsWithName
        then do
            rawFunctionDefs <- gets getRawFunctionDefs
            let rawFunctionDefsWithName = filter (\rawFunctionDef -> Raw.getFunctionName rawFunctionDef == name) rawFunctionDefs
            when (null rawFunctionDefsWithName) $ failAtPosition pos $ "No Function with the following name: " ++ name
            let calledRawFunctionDef = head rawFunctionDefsWithName
            parameterEvals <- mapM consumeEval rawEvals
            let signature = fst <$> parameterEvals
            when (length signature /= length (Raw.getArgumentNames calledRawFunctionDef)) $ failAtPosition pos $ "The number of arguments to \"" ++ name ++ "\" doesn't match."
            _ <- consumeFunctionBody calledRawFunctionDef signature
            consumeFunctionCall (Raw.FunctionCall pos name rawEvals)
        else do
            (signature, parameterEvals) <- mapAndUnzipM consumeEval rawEvals
            functionDefsIndeciesWithSignature <- map fst <$> filterM (\(_, possibelSignature) ->
                matchSignature signature possibelSignature) (zip ([0..] :: [Int]) (getSignature <$> functionDefsWithName))
            case listToMaybe functionDefsIndeciesWithSignature of
                Nothing -> do
                    let showSignature superpositions = intercalate ", " <$> mapM showSuperposition superpositions
                    signatureString <- showSignature signature
                    failAtPosition pos $ "The type-signature (" ++ signatureString ++ ") of the arguments of \"" ++ name ++ "\" is invalid."
                Just index -> do
                   let calledFunctionDef = functionDefsWithName !! index
                   return (getReturnType calledFunctionDef, FunctionCall (fromJust $ elemIndex calledFunctionDef functionDefs) parameterEvals)

matchSignature :: [Superposition] -> [Superposition] -> ParserWithState Program Bool
matchSignature xs ys =
    if length xs == length ys
        then all isRight <$> zipWithM tryMergeSuperposition xs ys
        else return False

consumeFunctionBody :: Raw.FunctionDef -> [Superposition] -> ParserWithState Program ()
consumeFunctionBody (Raw.FunctionDef name argumentNames instructions) signature = do
    backup <- enterContext True $ zip signature argumentNames
    returnSuperposition <- addSuperposition AnyType
    oldFunctionDefs <- gets getFunctionDefs
    let functionIndex = length oldFunctionDefs
    let emptyFunctionDef = FunctionDef { getSignature = signature, getFunctionName = name, getReturnType = returnSuperposition, getContext = Context [] [] }
    modify (\program -> program { getCurrentReturnType = returnSuperposition, getFunctionDefs = oldFunctionDefs ++ [emptyFunctionDef] } )
    mapM_ consumeInstruction instructions
    context <- leaveContext backup
    newFunctionDefs <- gets getFunctionDefs
    let newFunctionDef = FunctionDef { getSignature = signature, getFunctionName = name, getReturnType = returnSuperposition, getContext = context }
    let finalFunctionDefs = replaceAt newFunctionDefs functionIndex newFunctionDef
    modify (\program -> program { getFunctionDefs = finalFunctionDefs })

assertType :: Superposition -> Raw.Eval -> ParserWithState Program (Superposition, Eval)
assertType expectedSuperposition rawEval = do
    (actualSuperposition, eval) <- consumeEval rawEval
    resultingSuperposition <- mergeSuperposition (Raw.getSourcePos rawEval) expectedSuperposition actualSuperposition
    case expectedSuperposition of
        SuperpositionReference index -> do
            finalIndex <- trace index
            changeSuperposition finalIndex resultingSuperposition
            return (expectedSuperposition, eval)
        _ -> return (resultingSuperposition, eval)

trace :: Int -> ParserWithState Program Int
trace index = do
    nextSuperposition <- getSuperposition index
    case nextSuperposition of
        (SuperpositionReference nextIndex) -> trace nextIndex
        _ -> return index

failAtPosition :: SourcePos -> String -> ParserWithState s a
failAtPosition pos message = lift (setPosition pos) >> lift (fail message)

mergeSuperposition :: SourcePos -> Superposition -> Superposition -> ParserWithState Program Superposition
mergeSuperposition pos superpositionX superpositionY = do
    result <- tryMergeSuperposition superpositionX superpositionY
    case result of
        (Left message) -> failAtPosition pos message
        (Right newSuperposition) -> addSuperposition newSuperposition

tryMergeSuperposition :: Superposition -> Superposition -> ParserWithState Program (Either String Superposition)
tryMergeSuperposition Generic superpositionY = do
    generic <- gets getGeneric
    result <- tryMergeSuperposition generic superpositionY
    case result of
        Left message -> return $ Left message
        Right newGeneric -> do
            modify (\program -> program { getGeneric = newGeneric })
            return $ Right newGeneric
tryMergeSuperposition (SuperpositionReference index) superpositionY = do
    referencedSuperposition <- getSuperposition index
    tryMergeSuperposition referencedSuperposition superpositionY
tryMergeSuperposition superpositionX (SuperpositionReference index) = do
    referencedSuperposition <- getSuperposition index
    tryMergeSuperposition superpositionX referencedSuperposition
tryMergeSuperposition superpositionX AnyType = if superpositionX == Superposition [] then return $ Left "Value has no type." else return $ Right superpositionX
tryMergeSuperposition AnyType superpositionY = if superpositionY == Superposition [] then return $ Left "Value has no type." else return $ Right superpositionY
tryMergeSuperposition (Superposition xTypes) (Superposition yTypes) = do
    let types = [xType | xType <- xTypes, xType `elem` yTypes]
    if null types
        then do
            xTypesString <- showSuperposition $ Superposition xTypes
            yTypesString <- showSuperposition $ Superposition yTypes
            return $ Left $ "Failed to match type " ++ xTypesString ++ " with " ++ yTypesString ++ "."
        else do
            return $ Right $ Superposition types
tryMergeSuperposition (SuperpositionArray superpositionX) (SuperpositionArray superpositionY) = mapRight SuperpositionArray <$> tryMergeSuperposition superpositionX superpositionY
tryMergeSuperposition superpositionX superpositionY = do
    superpositionXString <- showSuperposition superpositionX
    superpositionYString <- showSuperposition superpositionY
    return $ Left $ "Failed to match " ++ superpositionXString ++ " with " ++ superpositionYString ++ "."

replaceAt :: [a] -> Int -> a -> [a]
replaceAt xs index x = part1 ++ [x] ++ tail part2
    where (part1, part2) = splitAt index xs

showSuperposition :: Superposition -> ParserWithState Program String
showSuperposition (SuperpositionReference index) = getSuperposition index >>= showSuperposition
showSuperposition AnyType = return "Anything"
showSuperposition Generic = return "T"
showSuperposition (Superposition types) = return $ intercalate " or " $ show <$> types
showSuperposition (SuperpositionArray superpositions) = do
    stringSuperposition <- showSuperposition superpositions
    return $ "[" ++ stringSuperposition ++ "]"

mapRight :: (b -> c) -> Either a b -> Either a c
mapRight f (Right x) = Right $ f x
mapRight _ (Left x) = Left x

changeSuperposition :: Int -> Superposition -> ParserWithState Program ()
changeSuperposition index newSuperposition = do
    currentTypeSuperpositions <- gets getSuperpositions
    modify (\program -> program { getSuperpositions = replaceAt currentTypeSuperpositions index newSuperposition })

getSuperposition :: Int -> ParserWithState Program Superposition
getSuperposition index = gets $ (!! index) . getSuperpositions

dereferenceSuperposition :: Superposition -> ParserWithState Program Superposition
dereferenceSuperposition (SuperpositionReference index) = do
    superposition <- getSuperposition index
    dereferenceSuperposition superposition
dereferenceSuperposition x = return x

addSuperposition :: Superposition -> ParserWithState Program Superposition
addSuperposition superposition = do
    currentTypeSuperpositions <- gets getSuperpositions
    modify (\program -> program { getSuperpositions = currentTypeSuperpositions ++ [superposition] })
    return $ SuperpositionReference $ length currentTypeSuperpositions

writeMainContext :: ParserWithState Program ()
writeMainContext = do
    newMainContext <- gets (head . getCurrentContexts)
    modify (\program -> program { getGlobalContext = newMainContext })

incrementLine :: ParserWithState Program ()
incrementLine = do
    line <- gets getCurrentLine
    modify (\program -> program { getCurrentLine = line + 1 })

getNewOffset :: ParserWithState Program Offset
getNewOffset = do
    isMainContext <- gets getIsMainContext
    if isMainContext then do
        currentGlobal <- gets getCurrentGlobal
        modify (\program -> program { getCurrentGlobal = currentGlobal + 1 })
        return $ OffsetName currentGlobal
    else do
        (Offset offset) <- gets getCurrentOffset
        modify (\program -> program { getCurrentOffset = Offset $ offset + 1 })
        return $ Offset offset

addVariableDef :: Superposition -> String -> ParserWithState Program ()
addVariableDef superposition name = do
    line <- gets getCurrentLine
    isMainContext <- gets getIsMainContext
    offset <- if isMainContext then do
            currentGlobal <- gets getCurrentGlobal
            modify (\program -> program { getCurrentGlobal = currentGlobal + 1 })
            return $ OffsetName currentGlobal
        else do
            (Offset offset) <- gets getCurrentOffset
            modify (\program -> program { getCurrentOffset = Offset $ offset + 1 })
            return $ Offset offset
    let variableDef = VariableDef { getType = superposition, getLineNumber = line, getVariableName = name, getVariableOffset = offset }
    currentContexts <- gets getCurrentContexts
    let modifyContext (Context existingVariableDefs instructions) = Context (existingVariableDefs ++ [variableDef]) instructions
    let newContexts = modifyContext (head currentContexts) : tail currentContexts
    modify (\program -> program { getCurrentContexts = newContexts })

addStringLiteral :: String -> ParserWithState Program Int
addStringLiteral literal = do
    stringLiterals <- gets getStringLiterals
    let result = length stringLiterals
    let newStringLiterals = stringLiterals ++ [literal]
    modify (\program -> program { getStringLiterals = newStringLiterals })
    return result

addFloatLiteral :: Double -> ParserWithState Program Int
addFloatLiteral literal = do
    floatLiterals <- gets getFloatLiterals
    let result = length floatLiterals
    let newFloatLiterals = floatLiterals ++ [literal]
    modify (\program -> program { getFloatLiterals = newFloatLiterals })
    return result

addInstruction :: Instruction -> ParserWithState Program ()
addInstruction instruction = do
    currentContexts <- gets getCurrentContexts
    let modifyContext (Context variableDefs instructions) = Context variableDefs (instructions ++ [instruction])
    let newContexts = modifyContext (head currentContexts) : tail currentContexts
    modify (\program -> program { getCurrentContexts = newContexts })

enterContext :: Bool -> [(Superposition, String)] -> ParserWithState Program Backup
enterContext clearPreviousContexts typeNamePairs = do
    backup <- getBackup
    currentContexts <- gets getCurrentContexts
    let newContext = Context [] []
    let newContexts = if clearPreviousContexts then [newContext] else newContext : currentContexts
    modify (\program -> program { getCurrentContexts = newContexts })
    when clearPreviousContexts $ modify (\program -> program { getIsMainContext = False, getCurrentOffset = Offset $ (-1) - length typeNamePairs })
    mapM_ (uncurry addVariableDef) typeNamePairs
    when clearPreviousContexts $ modify (\program -> program { getCurrentOffset = Offset 1 })
    incrementLine
    return backup

leaveContext :: Backup -> ParserWithState Program Context
leaveContext backup = do
    result <- gets (head . getCurrentContexts)
    recoverFromBackup backup
    return result

getBackup :: ParserWithState Program Backup
getBackup = do
    returnType <- gets getCurrentReturnType
    contexts <- gets getCurrentContexts
    offset <- gets getCurrentOffset
    isMainContext <- gets getIsMainContext
    return (returnType, contexts, offset, isMainContext)

recoverFromBackup :: Backup -> ParserWithState Program ()
recoverFromBackup (returnType, contexts, offset, isMainContext) = modify (\program -> program {
    getCurrentReturnType = returnType,
    getCurrentContexts = contexts,
    getCurrentOffset = offset,
    getIsMainContext = isMainContext })

getVariable :: SourcePos -> String -> ParserWithState Program VariableDef
getVariable pos name = do
    variableDefsWithName <- getRelevantVariableDefsWithName name
    if null variableDefsWithName
        then failAtPosition pos ("No Variable with the following name: " ++ name)
        else return $ last variableDefsWithName

getRelevantVariableDefsWithName :: String -> ParserWithState Program [VariableDef]
getRelevantVariableDefsWithName name = do
    line <- gets getCurrentLine
    let sortAndFilter variableDefs = sortOn getLineNumber $ filter (\variableDef -> getLineNumber variableDef < line && getVariableName variableDef == name) variableDefs
    localVariableDefs <- gets (sortAndFilter . concatMap getVariableDefs . getCurrentContexts)
    globalVariableDefs <- gets (sortAndFilter . getVariableDefs . getGlobalContext)
    return $ globalVariableDefs ++ localVariableDefs