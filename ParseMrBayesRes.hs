import LexCombinators
import Parsers (intStr, newick_tree_, realnum)
import ParserCandies
import Data.Char
import Data.List (isPrefixOf, isSuffixOf)
import System.Environment
import System.IO
import System.Directory (getDirectoryContents)
import Control.Monad (mapM)
import qualified Data.Map as Map
import ParseNexus (rubishPlusParser, findBest)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set

-- Extract translation map from taxon name to taxod id.
isBayesNexus :: String -> Bool
isBayesNexus fname =
	"sample.nexus.run" `isPrefixOf` fname && ".t" `isSuffixOf` fname

isBayesLikelihood fname =
	"sample.nexus.run" `isPrefixOf` fname && ".p" `isSuffixOf` fname

newline1 = many1 $ do {many $ sat ('\r'==); sat ('\n'==)}

newlineAny = many $ do {many $ sat ('\r'==); sat ('\n'==)}

parseTranslateBlock :: Parser (Map.Map String String)
parseTranslateBlock = do
						string "begin trees;"
						newline1
						space
						string "translate"
						trs <- translations
						return (Map.fromList trs)

translations :: Parser [(String, String)]
translations = do{
				newline1;
				space;
				t <- oneTrans;
				ts <- translations;
				return (t:ts)
				} +++ return []

oneTrans :: Parser (String, String)
oneTrans = do
			key <- token intStr
			val <- token wordOrUnderSc
			string "," +++ string ";"
			return (key, val)

rubishPlusTranslate :: Parser (Map.Map String String)
rubishPlusTranslate = do rubishPlusParser parseTranslateBlock

-- Extract the tree samples and translate the taxonId to taxon name
--
--Parse the trees part of the nexus file and return map of sample id -> nexus
--tree
parseTreesContent :: Map.Map String String -> Parser [(String, String)]
parseTreesContent trans_map = do{
								tl <- parseTreeLine trans_map;
								tls <- parseTreesContent trans_map;
								return (tl:tls)
								} +++ return []

parseTreeLine :: Map.Map String String -> Parser (String, String)
parseTreeLine trans_map = do
					newline1
					space
					token $ string "tree"
					token $ string "gen."
					id <- token $ intStr
					token $ sat ('='==)
					token $ string "[&U]"
					t <- newick_tree_ trans_map
					return (id, t)

parseTreesFile :: Parser (Map.Map String String)
parseTreesFile = do
					transMap <- rubishPlusTranslate
					treesAlist <- parseTreesContent transMap
					return (Map.fromList treesAlist)

-- Extract the treeId -> LnLikelihood from the text file produced by Mr.Bayes

parseLikelihood :: Parser (String, Double)
parseLikelihood = do
					treeId <- intStr
					space
					lnl <- realnum
					many $ do {space; realnum}
					newlineAny
					return (treeId, -lnl)

parseLikelihoodBlock :: Parser (Map.Map String Double)
parseLikelihoodBlock = do{
						(id, l) <- parseLikelihood;
						ls <- parseLikelihoodBlock;
						return (Map.insert id l ls)
						} +++ return Map.empty

parseLikelihoodFile :: Parser (Map.Map String Double)
parseLikelihoodFile = do
					-- first line
						string "[ID: "
						intStr
						string "]"
						newline1
					-- second line
						token $ string "Gen"
						token $ string "LnL"
						rubishPlusParser newline1
					-- likelihood block
						parseLikelihoodBlock

-- join maps (treeId -> newick) and (treeId -> LnL)

catMaybes_ :: [Maybe a] -> [a]
catMaybes_ [] = []
catMaybes_ (m:ms) = case m of
						Nothing -> error "kita"
						Just a -> a:(catMaybes_ ms)

joinMaps :: (Ord a, Ord b) => Map.Map a b -> Map.Map a c -> Map.Map b c
joinMaps m1 m2 = Map.fromList $ catMaybes $ map (joinOnKey m1 m2) (Map.keys m1)

joinToAssocList :: (Ord a) => Map.Map a b -> Map.Map a c -> [(b,c)]
joinToAssocList m1 m2 = catMaybes $ map (joinOnKey m1 m2) (Map.keys m1)

joinOnKey :: (Ord a) => Map.Map a b -> Map.Map a c -> a -> Maybe (b, c)
joinOnKey m1 m2 k = do
						v1 <- Map.lookup k m1
						v2 <- Map.lookup k m2
						return (v1, v2)

-- File handling utilities

mapParser :: Parser a -> [String] -> [a]
mapParser p [] = []
mapParser p (cs:css)= case parse p cs of
						[] -> mapParser p css
						((r,_):_) -> (r : (mapParser p css))

idFilenameT :: Parser (String, String)
idFilenameT = do
				nm <- word1
				dt <- string "."
				extension <- word1
				pf <- string ".run"
				i <- intStr
				sf <- string ".t"
				parseNothing
				return (i, nm ++ dt ++ extension ++ pf ++ i ++ sf)

idFilenameL :: Parser (String, String)
idFilenameL = do
				nm <- word1
				dt <- string "."
				extension <- word1
				pf <- string ".run"
				i <- intStr
				sf <- string ".p"
				parseNothing
				return (i, nm ++ dt ++ extension ++ pf ++ i ++ sf)

parseNothing :: Parser String
parseNothing = Parser (\cs -> if length cs == 0 then [("", "")] else [])

readFileMap :: [(String, String)] -> IO ([(String, String)])
readFileMap [] = return []
readFileMap ((k, v):es) = do
							c <- readFile v
							cs <- readFileMap es
							return ((k,c):cs)

attachParentDir :: String -> [(String, String)] -> [(String, String)]
attachParentDir indir [] = []
attachParentDir indir ((k, v):es) = ((k, indir++v):(attachParentDir indir es))

joinTreesLiksConts :: String -> String -> [(Double, String)]
joinTreesLiksConts tcon lcon =
	joinToAssocList liksMap treesMap
		where
			treesMap = fst $ head $ parse parseTreesFile tcon
			liksMap = fst $ head $ parse parseLikelihoodFile lcon

showBestTree :: [(Double, String)] -> String
showBestTree [] = "no trees"
showBestTree (t:ts) = "best likelihood:\n" ++ (show $ fst t) ++
	"\nbest tree:\n" ++ (show $ snd t)

main = do
	args <- getArgs
	let inDir = args !! 0
	dirStuff <- getDirectoryContents inDir
	let bayesNexsMap = mapParser idFilenameT dirStuff
	let bayesLikMap = mapParser idFilenameL dirStuff
	let bayesNexsMapFnames = attachParentDir inDir bayesNexsMap
	bayesNexsMapContent <- readFileMap bayesNexsMapFnames
	let bayesLikMapFnames = attachParentDir inDir bayesLikMap
	bayesLikMapContent <- readFileMap bayesLikMapFnames
	let treeLikContentMap = joinMaps (Map.fromList bayesNexsMapContent) (Map.fromList bayesLikMapContent)
	let likNewickMap = concat $ map (\(tc, lc) -> joinTreesLiksConts tc lc) $ Map.toAscList treeLikContentMap
	writeFile (args !! 1) $ showBestTree $ findBest [] likNewickMap


	--let bayesLikelihoods = filter isBayesLikelihood dirStuff
	-- Read the nexus file with trees
	--oneContent <- readFile $ (inDir++) $ bayesNexs !! 0
	--let treesMap = fst $ head $ parse (parseTreesFile) oneContent
	-- Read the file with likelihoods
	--likContent <- readFile $ (inDir++) $ bayesLikelihoods !! 0
	--let likMap = fst $ head $ parse parseLikelihoodFile likContent
	--let finalMap = joinMaps likMap treesMap
	--print ""


-- test code, delete after dubugging
parseLikelihoodBlockTest = do
					-- first line
						string "[ID: "
						intStr
						string "]"
						newline1
						token $ string "Gen"
						token $ string "LnL"
						rubishPlusParser newline1
						parseLikelihoodBlock

