import Text.ParserCombinators.Parsec
import System.Environment

-- First goal: Subset of Scheme R5
-- Syntax taken from https://people.csail.mit.edu/jaffer/r5rs_9.html
--
-- Syntax:
-- <program> -> <form>*
-- <form> -> <definition> | <expr>
-- <definition> -> (define <variable> <expr>)
-- <expr> -> <variable> | <literal> | <procedure-call>
-- <literal> -> <quotation> | <self-evaluating>
-- <quotation> -> '<datum> | (quote <datum>)
-- <self-evaluating> -> <boolean> | <number> | <string>
-- <procedure-call> -> (<operator> <operand>*)
-- <operator> -> <expr>
-- <operand> -> <expr>


-- data Form = Definition Identifier Expr

data Datum = Boolean Bool
            | Number Integer
            | Identifier String
            | List [Datum]
            | String String
            deriving Show

main :: IO ()
main = do
        x:xs <- getArgs
        putStrLn $ testParser x

testParser input = case res of
    Left err -> "No match: " ++ show err
    Right (String val) -> "Found string: " ++ val
    Right (List val) -> "Found list: " ++ show val
    _ -> "Found value but not implemented output yet"
    where res = parse parseExpr "test" input

parseExpr :: Parser Datum
parseExpr = parseString
        <|> parseList
        <|> parseIdentifier
        <?> "lisp expression"

parseString :: Parser Datum
parseString = do
            char '"'
            string <- many (noneOf "\"")
            char '"'
            return $ String string

parseList :: Parser Datum
parseList = do
            char '('
            list <- parseExpr `sepBy` (char ' ') 
            char ')'
            return $ List list

symbol = oneOf "!@#$%^&*-_+/:<=>?~"
parseIdentifier :: Parser Datum
parseIdentifier = do
            head <- symbol <|> letter
            tail <- many (symbol <|> letter <|> digit)
            return $ Identifier $ head:tail
