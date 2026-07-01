module Berat (Berat(..), menuBerat, konversiBerat) where

import IOUtils (getInputRaw, getInputClean)

-- | Custom Type untuk satuan Berat (Chapter 7)
data Berat = Kilogram | Gram | Ons | Pounds deriving (Show, Eq)

-- | Konversi ke satuan dasar (Gram) menggunakan Pattern Matching
keGram :: Double -> Berat -> Double
keGram nilai Kilogram = nilai * 1000.0
keGram nilai Gram     = nilai
keGram nilai Ons      = nilai * 100.0
keGram nilai Pounds   = nilai * 453.592

-- | Konversi dari satuan dasar (Gram) ke satuan tujuan
dariGram :: Double -> Berat -> Double
dariGram nilai Kilogram = nilai / 1000.0
dariGram nilai Gram     = nilai
dariGram nilai Ons      = nilai / 100.0
dariGram nilai Pounds   = nilai / 453.592

-- | Fungsi utama konversi: dari -> ke -> nilai -> hasil
konversiBerat :: Berat -> Berat -> Double -> Double
konversiBerat dari ke nilai = dariGram (keGram nilai dari) ke

-- | Parsing input string ke tipe Berat menggunakan Pattern Matching
parseBerat :: String -> Maybe Berat
parseBerat "KG"       = Just Kilogram
parseBerat "KILOGRAM" = Just Kilogram
parseBerat "G"        = Just Gram
parseBerat "GRAM"     = Just Gram
parseBerat "ONS"      = Just Ons
parseBerat "OZ"       = Just Ons
parseBerat "LB"       = Just Pounds
parseBerat "LBS"      = Just Pounds
parseBerat "POUNDS"   = Just Pounds
parseBerat _          = Nothing

tampilkanDaftarSatuan :: IO ()
tampilkanDaftarSatuan = do
    putStrLn "\n  Satuan berat yang tersedia:"
    putStrLn "  -------------------------"
    putStrLn "  [KG]  Kilogram"
    putStrLn "  [G]   Gram"
    putStrLn "  [ONS] Ons (100g)"
    putStrLn "  [LBS] Pounds"
    putStrLn "  -------------------------"

-- | Menu interaktif yang dipanggil oleh Main.hs
menuBerat :: IO ()
menuBerat = do
    putStrLn "\n========= KONVERSI BERAT ========="
    tampilkanDaftarSatuan
    inputDari  <- getInputClean "\n  Dari (contoh: KG)  > "
    inputKe    <- getInputClean "  Ke   (contoh: LBS) > "
    inputNilai <- getInputRaw   "  Jumlah              > "
    prosesKonversiBerat inputDari inputKe inputNilai

-- | Validasi input dan tampilkan hasil menggunakan Pattern Matching tuple
prosesKonversiBerat :: String -> String -> String -> IO ()
prosesKonversiBerat inputDari inputKe inputNilai =
    case (parseBerat inputDari, parseBerat inputKe, reads inputNilai) of
        (Nothing, _, _) ->
            putStrLn $ "\n  [!] Satuan berat '" ++ inputDari ++ "' tidak dikenali."
        (_, Nothing, _) ->
            putStrLn $ "\n  [!] Satuan berat '" ++ inputKe ++ "' tidak dikenali."
        (_, _, []) ->
            putStrLn "\n  [!] Jumlah yang dimasukkan bukan angka valid."
        (Just dari, Just ke, (nilai, _):_) ->
            let hasil = konversiBerat dari ke nilai
            in putStrLn $ "\n  Hasil: "
                       ++ show nilai  ++ " " ++ show dari
                       ++ "  =  "
                       ++ show hasil ++ " " ++ show ke