module Main (main) where

import Lib
import System.Environment (getArgs)
import GHC.IO.Encoding (setLocaleEncoding, utf8, getLocaleEncoding)
import System.IO.CodePage (withCP65001)

main :: IO ()
main = withCP65001 $ do
    enc <- getLocaleEncoding
    print enc
    setLocaleEncoding utf8
    args <- getArgs
    case args of
        [itemsFile, cartFile] -> runProgram itemsFile cartFile Nothing
        [itemsFile, cartFile, bonusFile] -> runProgram itemsFile cartFile (Just bonusFile)
        _ -> putStrLn "Введите: items.txt cart.txt bonus.txt(при наличии)"
