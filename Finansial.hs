module Finansial (MataUang(..), menuMataUang) where

import IOUtils (getInputRaw, getInputClean)

-- | Tipe data untuk mata uang yang didukung.
data MataUang
    = USD
    | EUR
    | SGD
    | IDR
    deriving (Show, Eq, Enum, Bounded)

-- | Representasi sebuah entri kurs: nama lengkap dan nilai terhadap IDR.
data EntriKurs = EntriKurs
    { simbolKurs :: MataUang
    , namaKurs   :: String
    , nilaiKeIdr :: Double
    } deriving (Show)

-- | Tabel kurs terpusat. Semua konversi mengacu ke sini (Offline-First).
daftarKurs :: [EntriKurs]
daftarKurs =
    [ EntriKurs USD "US Dollar"          16200.0
    , EntriKurs EUR "Euro"               17500.0
    , EntriKurs SGD "Singapore Dollar"   12100.0
    , EntriKurs IDR "Indonesian Rupiah"  1.0
    ]

-- | Cari entri kurs berdasarkan simbol menggunakan filter (Chapter 5).
cariKurs :: MataUang -> Maybe EntriKurs
cariKurs target =
    case filter (\e -> simbolKurs e == target) daftarKurs of
        []    -> Nothing
        (x:_) -> Just x

-- | Konversi nilai antar dua mata uang melalui IDR sebagai perantara.
konversiMataUang :: MataUang -> MataUang -> Double -> Maybe Double
konversiMataUang dari ke nilai = do
    entryDari <- cariKurs dari
    entryKe   <- cariKurs ke
    return ((nilai * nilaiKeIdr entryDari) / nilaiKeIdr entryKe)

-- | Parsing input string menjadi MataUang menggunakan pattern matching.
parseMataUang :: String -> Maybe MataUang
parseMataUang "USD" = Just USD
parseMataUang "EUR" = Just EUR
parseMataUang "SGD" = Just SGD
parseMataUang "IDR" = Just IDR
parseMataUang _     = Nothing

-- | Tampilkan semua mata uang yang tersedia beserta kursnya terhadap IDR.
tampilkanDaftarKurs :: IO ()
tampilkanDaftarKurs = do
    putStrLn "\n  Mata uang yang tersedia:"
    putStrLn "  -------------------------"
    mapM_ cetakEntri daftarKurs
    putStrLn "  -------------------------"
  where
    cetakEntri e =
        putStrLn $ "  [" ++ show (simbolKurs e) ++ "]"
                ++ "  " ++ namaKurs e
                ++ "  =  Rp " ++ show (nilaiKeIdr e)

-- | Menu interaktif konversi mata uang.
menuMataUang :: IO ()
menuMataUang = do
    putStrLn "\n========= KONVERSI MATA UANG ========="
    tampilkanDaftarKurs
    inputDari <- getInputClean "\n  Dari (contoh: USD) > "
    inputKe   <- getInputClean "  Ke   (contoh: EUR) > "
    inputNilai <- getInputRaw  "  Jumlah             > "
    prosesKonversi inputDari inputKe inputNilai

-- | Proses dan validasi seluruh input, lalu tampilkan hasil.
prosesKonversi :: String -> String -> String -> IO ()
prosesKonversi inputDari inputKe inputNilai =
    case (parseMataUang inputDari, parseMataUang inputKe, reads inputNilai) of
        (Nothing, _, _) ->
            putStrLn $ "\n  [!] Mata uang '" ++ inputDari ++ "' tidak dikenali."
        (_, Nothing, _) ->
            putStrLn $ "\n  [!] Mata uang '" ++ inputKe ++ "' tidak dikenali."
        (_, _, []) ->
            putStrLn "\n  [!] Jumlah yang dimasukkan bukan angka valid."
        (Just dari, Just ke, (nilai, _):_) ->
            case konversiMataUang dari ke nilai of
                Nothing ->
                    putStrLn "\n  [!] Konversi gagal. Periksa kembali input Anda."
                Just hasil ->
                    putStrLn $ "\n  Hasil: "
                             ++ show nilai  ++ " " ++ show dari
                             ++ "  =  "
                             ++ show hasil ++ " " ++ show ke