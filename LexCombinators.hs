module LexCombinators where
import ParsersAndCandies
import Data.Char (isAlphaNum)

isSpace :: Char -> Bool
isSpace ch = case ch of
                ' ' -> True
                '\t' -> True
                _ -> False

space :: Parser String
space = many (sat isSpace)

word :: Parser String
word = many (sat isAlphaNum)

word1 :: Parser String
word1 = many1 (sat isAlphaNum)

token :: Parser a -> Parser a
token p = do
            a <- p
            space
            return a

symb :: String -> Parser String
symb cs = token (string cs)

apply :: Parser a -> String -> [(a, String)]
apply p = parse (do {space; p})

