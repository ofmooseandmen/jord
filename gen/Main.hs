import System.Environment (getArgs)
import Text.ParserCombinators.ReadP (ReadP, many1, readP_to_S)

import qualified Ellipsoids as E
import Generator
import qualified Models as M
import qualified Parsers as P
import qualified Transformations as T

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inDir, outDir] -> do
            ellipsoidsModule <- process (inDir ++ "/ellipsoids.txt") outDir E.parser E.generator
            _ <- process (inDir ++ "/models.txt") outDir M.parser (M.generator ellipsoidsModule)
            _ <- process (inDir ++ "/transformations.txt") outDir T.parser T.generator
            return ()
        _ -> putStrLn ("Invalid arguments: " ++ show args)

process :: FilePath -> FilePath -> ReadP a -> Generator a -> IO String
process inf outd p g = do
    r <- parse p <$> readFile inf
    case r of
        Just (h, ps) -> do
            writeFile (outf outd h) (generate h g ps)
            return (module' h)
        Nothing -> error ("invalid definition in " ++ show inf)

parse :: ReadP a -> String -> Maybe (Header, [a])
parse p s =
    case map fst $ filter (null . snd) $ readP_to_S (parser p) s of
        [] -> Nothing
        rs:_ -> Just rs

parser :: ReadP a -> ReadP (Header, [a])
parser p = do
    hc <- P.comment
    m <- P.module'
    P.eol
    es <- many1 p
    return (Header hc m, es)

outf :: FilePath -> Header -> FilePath
outf d h = d ++ "/" ++ toPath (module' h) ++ ".hs"

toPath :: String -> FilePath
toPath m =
    let repl '.' = '/'
        repl c = c
     in map repl m
