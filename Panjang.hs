module Panjang (Panjang(..), menuPanjang, konversiPanjang) where

import IOUtils (getInputRaw, getInputClean)

-- | Tipe data untuk satuan Panjang.
data Panjang = KM | Meter | CM | Mile deriving (Show, Eq)

-- | Konversi input ke satuan dasar (Meter).
keMeter :: Double -> Panjang -> Double
keMeter nilai KM    = nilai * 1000.0
keMeter nilai Meter = nilai
keMeter nilai CM    = nilai / 100.0
keMeter nilai Mile  = nilai * 1609.344

-- | Konversi dari satuan dasar (Meter) ke output tujuan.
dariMeter :: Double -> Panjang -> Double
dariMeter nilai KM    = nilai / 1000.0
dariMeter nilai Meter = nilai
dariMeter nilai CM    = nilai * 100.0
dariMeter nilai Mile  = nilai / 1609.344

-- | Fungsi utama konversi.
konversiPanjang :: Panjang -> Panjang -> Double -> Double
konversiPanjang dari ke nilai = dariMeter (keMeter nilai dari) ke

-- | Parsing input string menjadi tipe Panjang menggunakan Pattern Matching.
parsePanjang :: String -> Maybe Panjang
parsePanjang "KM"         = Just KM
parsePanjang "KILOMETER"  = Just KM
parsePanjang "METER"      = Just Meter
parsePanjang "M"          = Just Meter
parsePanjang "CM"         = Just CM
parsePanjang "CENTIMETER" = Just CM
parsePanjang "MILE"       = Just Mile
parsePanjang "MIL"        = Just Mile
parsePanjang _            = Nothing

tampilkanDaftarSatuan :: IO ()
tampilkanDaftarSatuan = do
    putStrLn "\n  Satuan panjang yang tersedia:"
    putStrLn "  -------------------------"
    putStrLn "  [KM]    Kilometer"
    putStrLn "  [METER] Meter"
    putStrLn "  [CM]    Centimeter"
    putStrLn "  [MILE]  Mile"
    putStrLn "  -------------------------"

-- | Menu interaktif yang akan dipanggil oleh Main.hs
menuPanjang :: IO ()
menuPanjang = do
    putStrLn "\n========= KONVERSI SATUAN PANJANG ========="
    tampilkanDaftarSatuan
    inputDari  <- getInputClean "\n  Dari (contoh: KM)    > "
    inputKe    <- getInputClean "  Ke   (contoh: METER) > "
    inputNilai <- getInputRaw   "  Jumlah               > "
    prosesKonversiPanjang inputDari inputKe inputNilai

-- | Proses dan validasi seluruh input menggunakan pattern matching tuple.
prosesKonversiPanjang :: String -> String -> String -> IO ()
prosesKonversiPanjang inputDari inputKe inputNilai =
    case (parsePanjang inputDari, parsePanjang inputKe, reads inputNilai) of
        (Nothing, _, _) ->
            putStrLn $ "\n  [!] Satuan panjang '" ++ inputDari ++ "' tidak dikenali."
        (_, Nothing, _) ->
            putStrLn $ "\n  [!] Satuan panjang '" ++ inputKe ++ "' tidak dikenali."
        (_, _, []) ->
            putStrLn "\n  [!] Jumlah yang dimasukkan bukan angka valid."
        (Just dari, Just ke, (nilai, _):_) ->
            let hasil = konversiPanjang dari ke nilai
            in putStrLn $ "\n  Hasil: "
                       ++ show nilai  ++ " " ++ show dari
                       ++ "  =  "
                       ++ show hasil ++ " " ++ show ke