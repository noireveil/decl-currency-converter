module Main where

import Data.Char (toUpper)
import System.IO (hFlush, stdout)

-- import Finansial
-- import Panjang

{-|
  Author  : Yasyfi
  Feature : Core I/O Handlers & Input Sanitization
  Desc    : Menerapkan fungsionalitas I/O dasar dan sanitasi input (Chapter 6).
            Menggunakan `map toUpper` untuk memastikan arsitektur kebal terhadap typo (case-insensitive).
-}
getInputRaw :: String -> IO String
getInputRaw promptText = do
    putStr promptText
    hFlush stdout
    getLine

getInputClean :: String -> IO String
getInputClean promptText = do
    putStr promptText
    hFlush stdout
    input <- getLine
    return (map toUpper input)

main :: IO ()
main = return ()