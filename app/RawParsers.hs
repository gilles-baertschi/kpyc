module RawParsers (programP) where

import qualified RawStructure as Raw
import Text.Parsec
import Text.Parsec.String ( Parser )
import Text.Parsec.Expr
import Data.Functor
import Data.Either
import Control.Monad.Identity (Identity)
import RawStructure (Instruction)

type Indentation = [Int]

evalP :: Parser Raw.Eval
evalP = mathP <|> arrayElementP <|> parensP <|> boolLiteralP <|> floatLiteralP  <|> intLiteralP <|> stringLiteralP <|> arrayLiteralP <|> functionEvalP <|> variableEvalP

parensP :: Parser Raw.Eval
parensP = try (between (char '(' <* spacesP) (spacesP *> char ')') evalP) <?> "parenticies"

stringLiteralP :: Parser Raw.Eval
stringLiteralP = try (Raw.StringLiteral <$> getPosition <*> (char '\"' *> many stringCharP <* char '\"')) <?> "string"

stringCharP :: Parser Char
stringCharP = (string "\\n" $> '\n') <|> (string "\\\"" $> '"') <|> satisfy (/= '\"')

boolLiteralP :: Parser Raw.Eval
boolLiteralP = try (Raw.BoolLiteral <$> getPosition <*> (read <$> (string "True" <|> string "False"))) <?> "bool"

intLiteralP :: Parser Raw.Eval
intLiteralP = try (Raw.IntLiteral <$> getPosition <*> (read <$> many1 digit)) <?> "int"

floatLiteralP :: Parser Raw.Eval
floatLiteralP = try (Raw.FloatLiteral <$> getPosition <*> (read <$> many1 digit <> string "." <> many1 digit)) <?> "float"

arrayLiteralP :: Parser Raw.Eval
arrayLiteralP = try $ Raw.ArrayLiteral <$> getPosition <*> between (char '[' <* spacesP) (spacesP *> char ']') (sepBy evalP (spacesP *> char ',' <* spacesP))

arrayElementP :: Parser Raw.Eval
arrayElementP = try $ Raw.ArrayElement <$> evalWithoutArrayElementAndMathP <*> between (char '[' <* spacesP) (spacesP *> char ']') evalP
    where evalWithoutArrayElementAndMathP = parensP <|> boolLiteralP <|> floatLiteralP  <|> intLiteralP <|> stringLiteralP <|> arrayLiteralP <|> functionEvalP <|> variableEvalP

functionCallP :: Parser Raw.FunctionCall
functionCallP = try $ Raw.FunctionCall <$> getPosition <*> nameP <*> between (char '(' <* spacesP) (spacesP *> char ')') (sepBy evalP (spacesP *> char ',' <* spacesP))

functionEvalP :: Parser Raw.Eval
functionEvalP = try $ Raw.FunctionEval <$> functionCallP

variableEvalP :: Parser Raw.Eval
variableEvalP = try $ Raw.VariableEval <$> getPosition <*> nameP

nameP :: Parser String
nameP = (return <$> (letter <|> char '_')) <> many (alphaNum <|> char '_') <?> "name"

spacesP :: Parser ()
spacesP = many spaceP $> () <?> "spaces"

spaceP :: Parser ()
spaceP = satisfy (\c -> c == ' ' || c == '\t') $> () <?> "space"

indentationP :: Indentation -> Parser ()
indentationP i = count (sum i) spaceP $> () <?> "indentation"

instructionP :: Indentation -> Parser Raw.Instruction
instructionP i = try $ indentationP i *> (executionP <|> assignmentP <|> returnP <|> ifP i <|> whileP i <|> forP i)

executionP :: Parser Raw.Instruction
executionP = try $ Raw.Execution <$> functionCallP <* endLineP False ';'

assignmentP :: Parser Raw.Instruction
assignmentP = try $ Raw.Assignment <$> nameP <*> (spacesP *> char '=' *> spacesP *> evalP <* endLineP False ';')

returnP :: Parser Raw.Instruction
returnP = try $ Raw.Return <$> getPosition <*> (string "return" *> spaceP *> evalP) <* endLineP False ';'

ifP :: Indentation -> Parser Raw.Instruction
ifP i = try $ do
    _ <- string "if" <* spacesP
    ifCondition <- evalP
    endLineP True ':'
    ifInstructions <- indentationSegmentP i
    (elifConditions, elifInstructions) <- unzip <$> many (try $ do
        _ <- indentationP i <* string "elif" <* spacesP
        elifCondition <- evalP
        endLineP True ':'
        elifInstructions <- indentationSegmentP i
        return (elifCondition, elifInstructions))
    elseInstructions <- option [] (try $ do
        _ <- indentationP i <* string "else" <* spacesP
        endLineP True ':'
        elseInstructions <- indentationSegmentP i
        return [elseInstructions])
    return $ Raw.If (ifCondition : elifConditions) (ifInstructions : elifInstructions ++ elseInstructions)

whileP :: Indentation -> Parser Raw.Instruction
whileP i = try $ do
    _ <- string "while" <* spacesP
    condition <- evalP
    endLineP True ':'
    instructions <- indentationSegmentP i
    return $ Raw.While condition instructions

forP :: Indentation -> Parser Raw.Instruction
forP i = try $ do
    _ <- string "for" <* spacesP
    name <- nameP <* spacesP
    _ <- string "in" <* spacesP
    array <- evalP
    endLineP True ':'
    instructions <- indentationSegmentP i
    return $ Raw.For name array instructions

indentationSegmentP :: Indentation -> Parser [Instruction]
indentationSegmentP i = do
    spaceCount <- length <$> lookAhead (many spaceP)
    if spaceCount <= sum i then fail "expecting indentation after ':'" else do
        let newIndentation = (spaceCount - sum i) : i
        many1 (instructionP newIndentation)

endLineP :: Bool -> Char -> Parser ()
endLineP required terminator = spacesP <* (if required then char terminator else char terminator <|> return ' ') <* newLinesP

newLinesP :: Parser ()
newLinesP = void (many1 (try (emptyLineP <* endOfLine))) <|> emptyLineP <* lookAhead eof
    where emptyLineP = spacesP <* (commentP <|> return ())

commentP :: Parser ()
commentP = string "#" *> many (satisfy (/= '\n')) $> ()

functionDefP :: Parser Raw.FunctionDef
functionDefP = try $ do
    _ <- string "def" <* spacesP
    name <- nameP
    parameters <- between (char '(' <* spacesP) (spacesP *> char ')' <* endLineP True ':') (sepBy nameP (spacesP *> char ',' <* spacesP))
    instructions <- indentationSegmentP []
    return $ Raw.FunctionDef name parameters instructions --FunctionDef [] name (Context (map (VariableDef PyVoid) parameters) instructions)

programP :: Parser Raw.Program
programP = do
    _ <- newLinesP <|> return ()
    (instructions, defs) <- partitionEithers <$> many (try ((Left <$> instructionP []) <|> (Right <$> functionDefP)))
    _ <- eof
    return $ Raw.Program defs instructions

table :: OperatorTable String () Identity Raw.Eval
table = [ [prefix "-" (\pos eval1 -> Raw.FunctionEval $ Raw.FunctionCall pos "-" [eval1])
        ,  prefix "not" (\pos eval1 -> Raw.FunctionEval $ Raw.FunctionCall pos "not" [eval1])]
        , [binary "and" (\pos eval1 eval2 -> Raw.FunctionEval $ Raw.FunctionCall pos "and" [eval1, eval2]) AssocLeft
        , binary "or" (\pos eval1 eval2 -> Raw.FunctionEval $ Raw.FunctionCall pos "or" [eval1, eval2]) AssocLeft]
        , [binary "*" (\pos eval1 eval2 -> Raw.FunctionEval $ Raw.FunctionCall pos "*" [eval1, eval2]) AssocLeft
        , binary "/" (\pos eval1 eval2 -> Raw.FunctionEval $ Raw.FunctionCall pos "/" [eval1, eval2]) AssocLeft]
        , [binary "+" (\pos eval1 eval2 -> Raw.FunctionEval $ Raw.FunctionCall pos "+" [eval1, eval2]) AssocLeft
        , binary "-" (\pos eval1 eval2 -> Raw.FunctionEval $ Raw.FunctionCall pos "-" [eval1, eval2]) AssocLeft]
        , [binary "==" (\pos eval1 eval2 -> Raw.FunctionEval $ Raw.FunctionCall pos "==" [eval1, eval2]) AssocLeft
        , binary "<=" (\pos eval1 eval2 -> Raw.FunctionEval $ Raw.FunctionCall pos "<=" [eval1, eval2]) AssocLeft
        , binary ">=" (\pos eval1 eval2 -> Raw.FunctionEval $ Raw.FunctionCall pos ">=" [eval1, eval2]) AssocLeft
        , binary "<" (\pos eval1 eval2 -> Raw.FunctionEval $ Raw.FunctionCall pos "<" [eval1, eval2]) AssocLeft
        , binary ">" (\pos eval1 eval2 -> Raw.FunctionEval $ Raw.FunctionCall pos ">" [eval1, eval2]) AssocLeft]]

mathP :: Parser Raw.Eval
mathP = try $ buildExpressionParser table evalWithoutMathP
    where evalWithoutMathP =  arrayElementP <|> parensP <|> boolLiteralP <|> floatLiteralP  <|> intLiteralP <|> stringLiteralP <|> arrayLiteralP <|> functionEvalP <|> variableEvalP

binary :: String -> (SourcePos -> a -> a -> a) -> Assoc -> Operator String () Identity a
binary name f = Infix (try $ f <$> getPosition <* spacesP <* string name <* spacesP)

prefix :: String -> (SourcePos -> a -> a) -> Operator String () Identity a
prefix name f = Prefix (try $ f <$> getPosition <* spacesP <* string name <* spacesP)