import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import System.Environment (getArgs)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (sort, isPrefixOf, find, partition)
import Control.Category ((>>>))
import Control.Monad.IO.Class (MonadIO(liftIO))
import System.Posix.Internals (fileType)
import System.FilePath (joinPath)

data FileStructure = File String | Directory String [FileStructure]
    deriving Show

traverseDirectory :: FilePath -> IO FileStructure
traverseDirectory path = do
    isFile <- doesFileExist path

    if isFile then
        return $ File path
    else do
        files <- listDirectory path
        structure <- mapM (traverseDirectory . joinPath . (:) path . (: [])) files
        return $ Directory path structure


printFileOrDirAtDepth :: String -> Int -> IO()
printFileOrDirAtDepth path depth = putStrLn $ replicate depth '\t' ++ path

printFileStructure' :: Int -> FileStructure -> IO ()
printFileStructure' depth (File path) = printFileOrDirAtDepth path depth
printFileStructure' depth (Directory path files) = do
    printFileOrDirAtDepth (path ++ "/") depth
    mapM_ (printFileStructure' (depth + 1)) files

printFileStructure :: FileStructure -> IO ()
printFileStructure = printFileStructure' 0

data Options = Options {
    path :: FilePath,
    flags :: Flags
}

data Flags = Flags
    { showHidden :: Bool
    , sortFiles :: Bool
    }

parseOptions :: [String] -> Options
parseOptions args =
    let (flags, arguments) = partition (isPrefixOf "-") args
        path = fromMaybe "." $ listToMaybe arguments
        combinedFlags = concatMap tail flags
    in Options {
        path = path,
        flags = Flags {
            showHidden = 'a' `elem` combinedFlags,
            sortFiles = 's' `elem` combinedFlags
        }
    }

main = do
    programArgs <- getArgs
    let options = parseOptions programArgs
    structure <- traverseDirectory (path options)
    printFileStructure structure

    -- files <- listDirectory (path options)
    -- let flags' = flags options

    -- mapM_ putStrLn
    --     . (if showHidden flags' then id else filter (not . isPrefixOf "."))
    --     . (if sortFiles flags' then sort else id)
    --     $ files
