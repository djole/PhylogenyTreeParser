module LexCombinators where
import ParserCandies
import Data.Char (isAlphaNum)

-- Return True if whitespace (space or tab) character
isSpace :: Char -> Bool
isSpace ch = case ch of
                ' ' -> True
                '\t' -> True
                _ -> False

-- Parse any number of spaces
space :: Parser String
space = many (sat isSpace)

-- Parse a sequence of any number of alphanumerical characters (a-Z, 0-9)
word :: Parser String
word = many (sat isAlphaNum)

wordOrUnderSc :: Parser String
wordOrUnderSc = many (sat isAlphaNumUnderscore)

-- True if the symbol is alphanumerical or '_'
isAlphaNumUnderscore c = isAlphaNum c || c == '_'

-- same as previous, only the parser has to be applied at least once
word1 :: Parser String
word1 = many1 (sat isAlphaNum)

-- parse and remove spaces in after of the token
token :: Parser a -> Parser a
token p = do
            a <- p
            space
            return a

-- Apply previous to Strings
symb :: String -> Parser String
symb cs = token (string cs)

-- Honestly, I don't know the point of this
apply :: Parser a -> String -> [(a, String)]
apply p = parse (do {space; p})

