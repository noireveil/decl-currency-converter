{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Aeson (FromJSON)
import Data.Char (toUpper)
import GHC.Generics (Generic)
import Network.HTTP.Simple (httpJSON, getResponseBody, parseRequest)
import qualified Data.Map as Map
import System.Exit (exitSuccess)
import Text.Printf (printf)
import Control.Monad (when)
import System.IO (hFlush, stdout)

-- Struktur data untuk menampung response JSON dari API Frankfurter
data CurrencyResponse = CurrencyResponse
    { amount :: Double
    , base   :: String
    , date   :: String
    , rates  :: Map.Map String Double
    } deriving (Show, Generic)

instance FromJSON CurrencyResponse

-- Mengambil data kurs real-time dari API berdasarkan kode mata uang
fetchRate :: String -> String -> IO (Maybe Double)
fetchRate from to = do
    let url = "https://api.frankfurter.app/latest?from=" ++ from ++ "&to=" ++ to
    request <- parseRequest url
    response <- httpJSON request
    
    let currencyData = getResponseBody response :: CurrencyResponse
        rateMap = rates currencyData
    
    return (Map.lookup to rateMap)

-- Merapikan format angka hasil konversi agar punya 2 angka di belakang koma
formatCurrency :: String -> Double -> String
formatCurrency currency value = printf "%.2f %s" value currency

-- Mengeksekusi penarikan API dan langsung mencetak hasilnya ke layar terminal
displayConversion :: String -> String -> Double -> IO ()
displayConversion from to amountToConvert = do
    putStrLn $ "\nMenghubungi API Frankfurter untuk kurs " ++ from ++ " ke " ++ to ++ "..."
    rateResult <- fetchRate from to
    
    case rateResult of
        Just rate -> do
            let convertedAmount = amountToConvert * rate
            putStrLn "========================================="
            putStrLn "            HASIL KONVERSI               "
            putStrLn "========================================="
            putStrLn $ formatCurrency from amountToConvert ++ " = " ++ formatCurrency to convertedAmount
            putStrLn $ "(Rate: 1 " ++ from ++ " = " ++ show rate ++ " " ++ to ++ ")"
            putStrLn "=========================================\n"
        Nothing -> 
            putStrLn "[-] Gagal mendapatkan data kurs. Pastikan kode mata uang valid.\n"

-- Fungsi pembantu agar tulisan putStr langsung muncul di terminal sebelum getLine
tanyaUser :: String -> IO String
tanyaUser teks = do
    putStr teks
    hFlush stdout
    getLine

-- Menampilkan menu interaktif utama yang terus berulang (rekursi)
mainMenu :: IO ()
mainMenu = do
    putStrLn "=== KONVERTER MATA UANG REAL-TIME ==="
    putStrLn "1. Konversi USD ke IDR"
    putStrLn "2. Konversi EUR ke IDR"
    putStrLn "3. Konversi Custom (Pilih Kode)"
    putStrLn "4. Keluar"
    
    pilihan <- tanyaUser "Pilih menu (1-4): "
    
    case pilihan of
        "1" -> do
            amountStr <- tanyaUser "Masukkan jumlah USD: "
            let amountVal = read amountStr :: Double
            displayConversion "USD" "IDR" amountVal
            mainMenu
            
        "2" -> do
            amountStr <- tanyaUser "Masukkan jumlah EUR: "
            let amountVal = read amountStr :: Double
            displayConversion "EUR" "IDR" amountVal
            mainMenu
            
        "3" -> do
            fromRaw <- tanyaUser "Masukkan kode mata uang asal (misal: USD, EUR, GBP): "
            toRaw <- tanyaUser "Masukkan kode mata uang tujuan (misal: IDR, JPY): "
            amountStr <- tanyaUser "Masukkan jumlah: "
            
            -- Pastikan input dari user dikonversi ke huruf kapital semua agar tidak error saat lookup API
            let fromCurrency = map toUpper fromRaw
                toCurrency   = map toUpper toRaw
                amountVal    = read amountStr :: Double
                
            displayConversion fromCurrency toCurrency amountVal
            mainMenu
            
        "4" -> do
            putStrLn "Keluar dari program. Terima kasih!"
            exitSuccess
            
        _ -> do
            putStrLn "[-] Pilihan tidak valid!\n"
            mainMenu

-- Entry point eksekusi program
main :: IO ()
main = mainMenu