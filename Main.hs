import Parsers

main = do
   args <- getArgs
   content <- readFile (args !! 0)
   let linesOfFiles = lines content
