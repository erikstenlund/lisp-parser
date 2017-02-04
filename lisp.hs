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

data ASTNode = Boolean Bool
            | Number Integer
            | Identifier String
            | List [ASTNode]
            | String String
            deriving Show

main :: IO ()
main = do
     x:xs <- getArgs
     putStrLn $ show $ readExpr x

readExpr input = case parse parseASTNode "test" input of
                 Left err -> String $ "No match"
                 Right val -> val

readExpr' (Left err)  = String $ "No match"
readExpr' (Right val) = val

eval :: ASTNode -> ASTNode
eval (List [Identifier "quote", val])      = val
eval (List [Identifier "define", id, val]) = List [id, val] -- Not done, should be added to somekind of state
eval (List (Identifier func : args))       = applyFunc func $ map eval args
eval val                                   = val

-- Parsers
parseASTNode :: Parser ASTNode
parseASTNode = parseString
             <|> parseList
             <|> parseIdentifier
             <|> parseNumber
             <|> parseQuotation
             <|> parseDefine
             <?> "lisp expression"

-- '<datum> or (quote <datum>), second solved by parseList
parseQuotation = do
               char '\''
               datum <- parseASTNode
               return $ List [Identifier "quote", datum]

parseDefine = do
            string "define"
            id <- parseIdentifier
            list <- parseASTNode
            return $ List [Identifier "define", id, list]

parseString = do
            char '"'
            string <- many (noneOf "\"")
            char '"'
            return $ String string

parseList = do
          char '('
          list <- parseASTNode `sepBy` (char ' ')
          char ')'
          return $ List list

parseIdentifier = do
                head <- symbol <|> letter
                tail <- many (symbol <|> letter <|> digit)
                let identifier = head:tail 
                return $ case identifier of
                         "#t" -> Boolean True 
                         "#f" -> Boolean False
                         _    -> Identifier identifier

parseNumber = fmap (Number . read) $ many1 digit -- Only integers, for now

-- Helpers
applyFunc func args = case lookup func primitiveFuncs of
                      Just val -> val args
                      Nothing -> Identifier "Err"

unpack (Number a) = a

primitiveFuncs :: [(String, [ASTNode] -> ASTNode)]
primitiveFuncs = [
                  ("+", binaryOperation (+)),
                  ("*", binaryOperation (*))
                 ]

binaryOperation :: (Integer -> Integer -> Integer) -> [ASTNode] -> ASTNode
binaryOperation op xs = Number $ foldl1 op $ map unpack xs

symbol = oneOf "!@#$%^&*-_+/:<=>?~"
