import LexCombinators
import ParserCandies
import Data.Char (ord, isDigit)
import TreeModel


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
                bd <- token $ many1 $ sat isDigit
                dt <- string "." +++ return ""
                ad <- many1 $ sat isDigit
                return (bd ++ dt ++ ad)


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
name = word

brLength :: Parser Double
brLength = do {string ":"; realnum} +++ return 0.0

