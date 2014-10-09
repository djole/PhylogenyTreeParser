module TreeModel where

data BinaryPhyloTree = Leaf {taxonName :: String,
                             branchLen :: Double}
                       | Node {label :: String,
                               branchLen :: Double,
                               support :: Maybe Double,
                               left :: BinaryPhyloTree,
                               right :: BinaryPhyloTree} deriving (Show)

makeINode :: BinaryPhyloTree -> BinaryPhyloTree -> String -> Double -> BinaryPhyloTree
makeINode left right label brLen = Node label brLen Nothing left right
