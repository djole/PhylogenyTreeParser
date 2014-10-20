import LexCombinators
import Parsers
import ParserCandies
import Data.Char
import System.Environment
import System.IO

rubishPlusParser_ :: Parser a -> String -> [(a, String)]
rubishPlusParser_  p "" = []
rubishPlusParser_  p cs = case parse p cs of
						[] -> rubishPlusParser_ p $ tail cs
						x -> x

rubishPlusParser p = Parser (rubishPlusParser_ p)

rubishPlusNewickTree = rubishPlusParser newick_tree


main = do
	args <- getArgs
	content <- readFile (args !! 0)
	let linesOfFile = lines content
	writeFile (args !! 1) $ parseNLines linesOfFile


parse1Line :: String -> String
parse1Line = \l -> show $ parseAndPeel parseTreeLine l

parseNLines :: [String] -> String
parseNLines fileLines = unlines $ filterNonTrees (map parse1Line fileLines)

filterNonTrees = filter (\ln -> ln /= "[]")

treeName :: Parser String
treeName = do
			string "'"
			tname <- many (sat (\c -> c /= '\''))
			string "'"
			return tname

treeComment :: Parser String
treeComment = do
				string "["
				tcomm <- many (sat (\c -> c /= '[' && c /= ']'))
				string "]"
				return tcomm

treeCommentLik :: Parser Double
treeCommentLik = do
					string "[Likelihood of "
					l <- realnum
					many (sat (\c -> c /= '[' && c /= ']'))
					string "]"
					return l

parseTreeLine :: Parser (Double, String)
parseTreeLine = do
					token (string "TREE")
					treeName
					space
					l <- treeCommentLik
					t <- rubishPlusNewickTree
					return (l, t)

