import System.Directory (listDirectory)
import System.Environment (getArgs)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (sort, isPrefixOf, find, partition)
import Control.Category ((>>>))

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

    files <- listDirectory (path options)
    let flags' = flags options

    mapM_ putStrLn
        . (if showHidden flags' then id else filter (not . isPrefixOf "."))
        . (if sortFiles flags' then sort else id)
        $ files
