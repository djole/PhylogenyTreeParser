import ParseNexus (parseNLines)
import System.Environment
import System.IO
import System.Directory (getDirectoryContents)

main = do
	args <- getArgs
	let inDir = args !! 0
	dirStuff <- getDirectoryContents inDir
	let files = filter (\x -> x /= "." && x /= "..") dirStuff
	contents <- mapM readFile (map (inDir++) files)
	let linesOfFiles = concat $map lines contents
	writeFile (args !! 1) $ show $ parseNLines linesOfFiles

