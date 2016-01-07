import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language
import Sound.Tidal.Context

{- Take in a pattern and extract the data -}

lexer :: TokenParser ()
lexer = makeTokenParser (javaStyle { identStart })

parseSyntax :: Parser ()
parseSyntax = do
    many $ noneOf "sound0123456789$"
    return ()

parseWhiteSpace :: Parser String
parseWhiteSpace = do
    whiteSpace lexer

parsePatternSound :: Parser String
parsePatternSound = do
    parseWhiteSpace
    parseSyntax
    

display :: String -> String
display s =
    case ret of
        Left e -> "error: " ++ (show e)
        Right n -> "answer: " ++ (show n)
    where
        ret = parsePatternSound "" s

main :: IO ()
main = interact (unlines . (map display) . lines)


