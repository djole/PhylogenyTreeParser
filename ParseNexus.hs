import LexCombinators
import Parsers
import ParserCandies
import Data.Char
import System.Environment

rubishPlusParser_ :: Parser a -> String -> [(a, String)]
rubishPlusParser_  p "" = []
rubishPlusParser_  p cs = case parse p cs of
						[] -> rubishPlusParser_ p $ tail cs
						x -> x

rubishPlusParser p = Parser (rubishPlusParser_ p)


main = do
	args <- getArgs
	content <- readFile (args !! 0)
	print $ show content

