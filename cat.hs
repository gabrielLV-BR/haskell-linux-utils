import Distribution.Simple.Utils (safeHead)
import System.Environment (getArgs)

main = do
    args <- getArgs
    let path = safeHead args
    case path of
        (Just file) -> do
            content <- readFile file
            putStrLn content
        Nothing -> fail "No file path provided"