module IOUtils (getInputRaw, getInputClean) where

import Data.Char (toUpper)
import System.IO (hFlush, stdout)

-- | Membaca input mentah dari user.
getInputRaw :: String -> IO String
getInputRaw promptText = do
    putStr promptText
    hFlush stdout
    getLine

-- | Membaca input dan menyamakannya ke huruf besar tanpa spasi.
getInputClean :: String -> IO String
getInputClean promptText = do
    putStr promptText
    hFlush stdout
    map toUpper . filter (/= ' ') <$> getLine