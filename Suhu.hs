module Suhu (Suhu(..), menuSuhu, konversiSuhu) where

import IOUtils (getInputRaw, getInputClean)

-- | Custom Type untuk satuan Suhu (Chapter 7)
data Suhu = Celcius | Fahrenheit | Kelvin | Reamur deriving (Show, Eq)

-- | Konversi ke satuan dasar (Celcius) menggunakan Pattern Matching
keCelcius :: Double -> Suhu -> Double
keCelcius nilai Celcius    = nilai
keCelcius nilai Fahrenheit = (nilai - 32) * 5 / 9
keCelcius nilai Kelvin     = nilai - 273.15
keCelcius nilai Reamur     = nilai * 5 / 4

-- | Konversi dari satuan dasar (Celcius) ke satuan tujuan
dariCelcius :: Double -> Suhu -> Double
dariCelcius nilai Celcius    = nilai
dariCelcius nilai Fahrenheit = (nilai * 9 / 5) + 32
dariCelcius nilai Kelvin     = nilai + 273.15
dariCelcius nilai Reamur     = nilai * 4 / 5

-- | Fungsi utama konversi: dari -> ke -> nilai -> hasil
konversiSuhu :: Suhu -> Suhu -> Double -> Double
konversiSuhu dari ke nilai = dariCelcius (keCelcius nilai dari) ke

-- | Parsing input string ke tipe Suhu menggunakan Pattern Matching
parseSuhu :: String -> Maybe Suhu
parseSuhu "C"           = Just Celcius
parseSuhu "CELCIUS"     = Just Celcius
parseSuhu "CELSIUS"     = Just Celcius
parseSuhu "F"           = Just Fahrenheit
parseSuhu "FAHRENHEIT"  = Just Fahrenheit
parseSuhu "K"           = Just Kelvin
parseSuhu "KELVIN"      = Just Kelvin
parseSuhu "R"           = Just Reamur
parseSuhu "REAMUR"      = Just Reamur
parseSuhu _             = Nothing

tampilkanDaftarSatuan :: IO ()
tampilkanDaftarSatuan = do
    putStrLn "\n  Satuan suhu yang tersedia:"
    putStrLn "  -------------------------"
    putStrLn "  [C]  Celcius"
    putStrLn "  [F]  Fahrenheit"
    putStrLn "  [K]  Kelvin"
    putStrLn "  [R]  Reamur"
    putStrLn "  -------------------------"

-- | Menu interaktif yang dipanggil oleh Main.hs
menuSuhu :: IO ()
menuSuhu = do
    putStrLn "\n========= KONVERSI SUHU ========="
    tampilkanDaftarSatuan
    inputDari  <- getInputClean "\n  Dari (contoh: C) > "
    inputKe    <- getInputClean "  Ke   (contoh: F) > "
    inputNilai <- getInputRaw   "  Jumlah            > "
    prosesKonversiSuhu inputDari inputKe inputNilai

-- | Validasi input dan tampilkan hasil menggunakan Pattern Matching tuple
prosesKonversiSuhu :: String -> String -> String -> IO ()
prosesKonversiSuhu inputDari inputKe inputNilai =
    case (parseSuhu inputDari, parseSuhu inputKe, reads inputNilai) of
        (Nothing, _, _) ->
            putStrLn $ "\n  [!] Satuan suhu '" ++ inputDari ++ "' tidak dikenali."
        (_, Nothing, _) ->
            putStrLn $ "\n  [!] Satuan suhu '" ++ inputKe ++ "' tidak dikenali."
        (_, _, []) ->
            putStrLn "\n  [!] Jumlah yang dimasukkan bukan angka valid."
        (Just dari, Just ke, (nilai, _):_) ->
            let hasil = konversiSuhu dari ke nilai
            in putStrLn $ "\n  Hasil: "
                       ++ show nilai  ++ " " ++ show dari
                       ++ "  =  "
                       ++ show hasil ++ " " ++ show ke