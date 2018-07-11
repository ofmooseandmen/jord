-- |
-- Module:      Main
-- Copyright:   (c) 2018 Cedric Liegeois
-- License:     BSD3
-- Maintainer:  Cedric Liegeois <ofmooseandmen@yahoo.fr>
-- Stability:   experimental
-- Portability: portable
--
-- REPL around "Jord".
--
module Main where

import Data.Geo.Jord
import Prelude hiding (lookup)
import System.IO

main :: IO ()
main = do
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
    putStrLn
        ("jord interpreter, version " ++
         jordVersion ++ ": https://github.com/ofmooseandmen/jord  :? for help")
    loop emptyVault
  where
    loop state = do
        input <- readI
        case input of
            ":quit" -> return ()
            ":q" -> return ()
            _ -> do
                let (result, newState) = evalS input state
                printS result
                loop newState

readI :: IO String
readI = putStr "\9783 " >> hFlush stdout >> getLine

printS :: Either String String -> IO ()
printS (Left err) = putStrLn ("\x1b[31m\9783 \x1b[30m" ++ err)
printS (Right "") = return ()
printS (Right r) = putStrLn ("\x1b[32m\9783 \x1b[30m" ++ r)

evalS :: String -> Vault -> (Either String String, Vault)
evalS s vault
    | null s = (Right "", vault)
    | head s == ':' = evalC w vault
    | (v:"=":e) <- w =
        let r = eval (unwords e) vault
            vault' = save r v vault
         in (showR r, vault')
    | otherwise = (showR (eval s vault), vault)
  where
    w = words s

evalC :: [String] -> Vault -> (Either String String, Vault)
evalC [":show", v] vault = (evalShow v vault, vault)
evalC [":delete", v] vault = evalDel (Just v) vault
evalC [":clear"] vault = evalDel Nothing vault
evalC [":help"] vault = (Right help, vault)
evalC [":?"] vault = (Right help, vault)
evalC c vault = (Left ("Unsupported command " ++ unwords c ++ "; :? for help"), vault)

evalShow :: String -> Vault -> Either String String
evalShow n vault = maybe (Left ("Unbound variable: " ++ n)) (Right . showVar n) (lookup n vault)

evalDel :: Maybe String -> Vault -> (Either String String, Vault)
evalDel (Just n) vault = (Right ("deleted var: " ++ n), delete n vault)
evalDel Nothing _ = (Right "deleted all variable ", emptyVault)

help :: String
help =
    "\njord interpreter, version " ++
    jordVersion ++
    "\n" ++
    "\n Commands available from the prompt:\n\n" ++
    "    :help, :?              display this list of commands\n" ++
    "    :quit, :q              quit jord\n" ++
    "    :show {var}            shows {var}\n" ++
    "    :delete {var}          deletes {var}\n" ++
    "    :clear                 deletes all variable(s)\n" ++
    "\n Jord expressions:\n\n" ++
    "    (f x y) where f is one of function described below and x and y\n" ++
    "    are either parameters in one of the format described below or\n" ++
    "    a call to another function\n" ++
    "\n" ++
    "    (finalBearing (destination (antipode 54°N,154°E) 54° 1000m) (readGeoPos 54°N,154°E))\n" ++
    "\n" ++
    "    Top level () can be ommitted: antipode 54N028E\n" ++
    "\n  Position calculations:\n\n" ++
    "       antipode pos               antipodal point of pos\n" ++
    "       distance pos1 pos2         surface distance between pos1 and pos2\n" ++
    "       destination pos len ang    destination position from pos having travelled len\n" ++
    "                                  on initial bearing ang\n" ++
    "       initialBearing pos1 pos2   bearing arriving at pos2 from pos1\n" ++
    "       finalBearing pos1 pos2     initial bearing from pos1 to pos2\n" ++
    "       decimal pos                decimal latitude and longitude of pos\n" ++
    "       decimal ang                decimal degrees of ang\n" ++
    "       midpoint [pos]             mid position between [pos]\n" ++
    "       toNVector pos              n-vector corresponding to pos\n" ++
    "\n  Supported Position formats:\n\n" ++
    "       DD(MM)(SS)[N|S]DDD(MM)(SS)[E|W] - 553621N0130209E\n" ++
    "       d°m's\"[N|S],d°m's\"[E|W]         - 55°36'21\"N,13°2'9\"E\n" ++
    "         ^ zeroes can be ommitted and separtors can also be d, m, s\n" ++
    "       decimal°[N|S],decimal°[E|W]     - 51.885°N,13,1°E\n" ++
    "\n  Supported Angle formats:\n\n" ++
    "       d°m's    - 55°36'21.154\n" ++
    "       decimal° - 51.885°\n" ++
    "\n  Supported Length formats: {l}m, {l}km, {l}Nm\n\n" ++
    "\n  Every evaluated result can be saved by prefixing the expression with \"{var} = \"\n" ++
    "  Saved results can subsequently be used when calling a function\n" ++
    "    \9783 a = antipode 54N028E\n" ++ "    \9783 antipode a\n"

save :: Result -> String -> Vault -> Vault
save (Right a@(Ang _)) k vault = insert k a vault
save (Right a@(AngDec _)) k vault = insert k a vault
save (Right l@(Len _)) k vault = insert k l vault
save (Right g@(Geo _)) k vault = insert k g vault
save (Right ll@(GeoDec _)) k vault = insert k ll vault
save (Right v@(Vec _)) k vault = insert k v vault
save _ _ vault = vault

showR :: Result -> Either String String
showR (Left err) = Left err
showR (Right v) = Right (showV v)

showV :: Value -> String
showV (Ang a) = "angle: " ++ show a
showV (AngDec a) = "angle (dd): " ++ show a
showV (Len l) = "length: " ++ show l
showV (Geo g) = "geo pos: " ++ show g
showV (GeoDec ll) = "latitude (dd): " ++ show (fst ll) ++ "; longitude (dd): " ++ show (snd ll)
showV (Vec v) = "n-vector: " ++ show v

showVar :: String -> Value -> String
showVar n v = n ++ "=" ++ showV v
