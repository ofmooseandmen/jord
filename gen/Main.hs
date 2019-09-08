import System.Environment (getArgs)
import System.IO (readFile, writeFile)
import Text.ParserCombinators.ReadP (ReadP, many1, readP_to_S)

import qualified Ellipsoids as E
import Generator
import qualified Models as M
import Parsers

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inDir, outDir] -> do
            ellipsoidsModule <- process (inDir ++ "/ellipsoids.txt") outDir E.parser E.generator
            _ <- process (inDir ++ "/models.txt") outDir M.parser (M.generator ellipsoidsModule)
            return ()
        _ -> putStrLn ("Invalid arguments: " ++ show args)

process :: FilePath -> FilePath -> ReadP a -> Generator a -> IO String
process inf outd p g = do
    mps <- parse p <$> readFile inf
    case mps of
        Just (m, ps) -> do
            writeFile (outf outd m) (generate m g ps)
            return m
        Nothing -> error ("invalid definition in " ++ show inf)

parse :: ReadP a -> String -> Maybe (String, [a])
parse p s =
    case map fst $ filter (null . snd) $ readP_to_S (parser p) s of
        [] -> Nothing
        rs:_ -> Just rs

parser :: ReadP a -> ReadP (String, [a])
parser p = do
    m <- module'
    eol
    es <- many1 p
    return (m, es)

outf :: FilePath -> String -> FilePath
outf d m = d ++ "/" ++ toPath m ++ ".hs"

toPath :: String -> FilePath
toPath m =
    let repl '.' = '/'
        repl c = c
     in map repl m