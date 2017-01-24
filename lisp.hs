import Text.ParserCombinators.Parsec
import System.Environment

data ASTNode = Atom
             | List [ASTNode]
             | String String
             deriving Show
data Atom = AtomNumber Integer | AtomString String

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

parseExpr :: Parser ASTNode
parseExpr = parseString
        <|> parseList
        <?> "lisp expression"

parseString :: Parser ASTNode
parseString = do
            char '"'
            string <- many (noneOf "\"")
            char '"'
            return $ String string

parseList :: Parser ASTNode
parseList = do
            char '('
            list <- parseExpr `sepBy` (char ' ') 
            char ')'
            return $ List list



