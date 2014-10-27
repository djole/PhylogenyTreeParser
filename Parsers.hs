module Parsers where

import LexCombinators
import ParserCandies
import Data.Char (ord, isDigit)
import TreeModel
import qualified Data.Map as Map


-- Implemented grammar for parsing arithmetic equations
expr :: Parser Int
addop :: Parser (Int -> Int -> Int)
mulop :: Parser (Int -> Int -> Int)

expr = term `chainl1` addop
term = factor `chainl1` mulop
factor = digit +++ do {symb "("; ex <- expr; symb ")"; return ex}
digit = do {x <- token $ many1 $ sat isDigit; return (read x)}

addop = do{symb "+"; return (+)} +++ do{symb "-"; return(-)}
mulop = do{symb "*"; return (*)} +++ do{symb "/"; return(div)}

--- Parse real number
realnum :: Parser Double
realnum = do { x <- realStr; return (read x)}

realStr :: Parser String
realStr = do
                sgn <- string "-" +++ string "+" +++ return ""
                bd <- many1 $ token $ sat isDigit
                dt <- string "." +++ return ""
                ad <- many1 $ sat isDigit
                ex <- parseExponent +++ return ""
                return (sgn ++ bd ++ dt ++ ad ++ ex)

intStr :: Parser String
intStr = many1 $ sat isDigit

parseExponent :: Parser String
parseExponent = do
					e <- string "E" +++ string "e"
					minus <- string "-" +++ string "+" +++ return ""
					n <- many1 $ sat isDigit
					return (e ++ minus ++ n)


-- Parse Newick formatted String
-- For the utilised grammar refer to:
-- http://www.bioperl.org/wiki/Newick_tree_format#Grammar
-- I modified the:
-- BranchSet --> Branch | Branch "," BranchSet
-- to:
-- BranchPair --> Branch "," Branch
-- because for the time, i want to apply it only to the binary trees
--
tree :: Parser BinaryPhyloTree
tree = do {t <- subtree; string ";"; return (t 0.0)}

subtree :: Parser (Double -> BinaryPhyloTree)
subtree =  internal +++ leaf

internal :: Parser (Double -> BinaryPhyloTree)
internal = do {
    string "(";
    (left, right) <- branchPair;
    string ")";
    lbl <- name;
    return (makeINode left right lbl)
    }

leaf :: Parser (Double -> BinaryPhyloTree)
leaf = do {nm <- name; return (Leaf nm)}

branchPair :: Parser ((BinaryPhyloTree, BinaryPhyloTree))
branchPair = do {
    left <- branch;
    string ",";
    right <- branch;
    return ((left, right))
    }

branch :: Parser BinaryPhyloTree
branch = do {inodeConst <- subtree; len <- brLength; return (inodeConst len)}

name :: Parser String
name = wordOrUnderSc

brLength :: Parser Double
brLength = do {string ":"; realnum} +++ return 0.0

-- Parse newick formatted tree
-- and return string that is a correctly formatted string.

newickTree = newick_tree_ Map.empty

newick_tree_ :: Map.Map String String -> Parser String
newick_tree_  trans_map = do{
						t <- newick_subtree trans_map;
						sc <- string ";";
						return (t ++ sc);
						}

newick_subtree :: Map.Map String String-> Parser String
newick_subtree trans_map = (newick_internal trans_map) +++ (newick_name trans_map)

newick_internal :: Map.Map String String -> Parser String
newick_internal trans_map = do {
					lb <- string "(";
					br_set <- newick_branch_set trans_map;
					rb <- string ")";
					nm <- newick_name trans_map;
					return (lb ++ br_set ++ rb ++ nm)
					}

newick_branch_set :: Map.Map String String -> Parser String
newick_branch_set trans_map= (newick_branch trans_map) `chainl1` comma

newick_branch :: Map.Map String String -> Parser String
newick_branch trans_map = do
	sub_t <- newick_subtree trans_map
	br_len <- newick_length
	return (sub_t ++ br_len)

newick_name :: Map.Map String String -> Parser String
newick_name trans_map = do
	id <- wordOrUnderSc
	let name = case Map.lookup id trans_map of
				Just x -> x
				Nothing -> id
	return name

newick_length :: Parser String
newick_length = do{col <- string ":"; n <- realStr; return (col ++ n)} +++ return ""

comma = do {symb ","; return (\x y -> x ++ "," ++ y)}

