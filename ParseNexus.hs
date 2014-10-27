module ParseNexus where
import LexCombinators
import Parsers
import ParserCandies
import Data.Char
import System.Environment
import System.IO
import System.Directory (getDirectoryContents)
import Control.Monad (mapM)
import Math.Statistics

data AnalysisStats = AnalysisStats {
	bestTree :: Maybe (Double, String),
	likelihoods :: [Double]
	}

showStats :: AnalysisStats -> String
showStats (AnalysisStats (Just bt) ls) =
	"best tree :\n" ++ (show bt) ++ "avg: " ++ show (average ls) ++ "stdev: "
		++ show (stddev ls)

instance Show AnalysisStats where
	show = showStats


rubishPlusParser_ :: Parser a -> String -> [(a, String)]
rubishPlusParser_  p "" = []
rubishPlusParser_  p cs = case parse p cs of
						[] -> rubishPlusParser_ p $ tail cs
						x -> x

rubishPlusParser p = Parser (rubishPlusParser_ p)

rubishPlusNewickTree = rubishPlusParser newickTree


main = do
	args <- getArgs
	let inDir = args !! 0
	dirStuff <- getDirectoryContents inDir
	let files = filter (\x -> x /= "." && x /= "..") dirStuff
	contents <- mapM readFile (map (inDir++) files)
	let linesOfFiles = concat $map lines contents
	writeFile (args !! 1) $ show $ parseNLines linesOfFiles


parse1Line :: String -> [(Double, String)]
parse1Line = \l -> parseAndPeel parseTreeLine l

parseNLines :: [String] -> AnalysisStats
parseNLines fileLines =
	gatherStats $ concat $ filterNonTrees $ map parse1Line fileLines

filterNonTrees :: [[(Double, String)]] -> [[(Double, String)]]
filterNonTrees = filter (\ln -> length ln /= 0)

gatherStats :: [(Double, String)] -> AnalysisStats
gatherStats data_ = gatherStats_ (AnalysisStats Nothing []) data_

gatherStats_ :: AnalysisStats -> [(Double, String)] -> AnalysisStats
gatherStats_ (AnalysisStats Nothing ls) [] = (AnalysisStats Nothing ls)
gatherStats_ analisSt [] = analisSt
gatherStats_ (AnalysisStats Nothing ls) (t:ts) =
	gatherStats_ (AnalysisStats (Just t) (fst t : ls)) ts
gatherStats_ (AnalysisStats (Just (bl, bt)) ls) ((l,t):ts) =
	if l < bl
		then gatherStats_ (AnalysisStats (Just (l, t)) (l:ls)) ts
		else gatherStats_ (AnalysisStats (Just (bl, bt)) (l:ls)) ts

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

