module Main where

import Data.Char   (toUpper)
import System.Exit (exitSuccess)

-- | Tipe alias untuk fungsi konversi, digunakan sebagai parameter HOF 
type FungsiKonversi = Double -> Double


main :: IO ()
main = do
  tampilkanSplashScreen
  mainMenu

tampilkanSplashScreen :: IO ()
tampilkanSplashScreen = do
  putStrLn "========================================"
  putStrLn "     KONVERTER SATUAN & MATA UANG"
  putStrLn "========================================"

-- ----------------------------------------------------------------
-- MENU UTAMA - REKURSI ]
-- Siklus aplikasi dipertahankan dengan rekursi: prosesMenu memanggil
-- mainMenu kembali di setiap cabang, tanpa while-loop.
-- ----------------------------------------------------------------

mainMenu :: IO ()
mainMenu = do
  putStrLn "\n============= MENU UTAMA ============="
  putStrLn "  [1] Konversi Mata Uang"
  putStrLn "  [2] Konversi Panjang"
  putStrLn "  [3] Konversi Suhu"
  putStrLn "  [4] Konversi Berat"
  putStrLn "  [5] Konversi Volume"
  putStrLn "  [6] Konversi Data Digital"
  putStrLn "  [7] Konversi Batch"
  putStrLn "  [K] Keluar"
  putStrLn "======================================="
  putStr "  Pilih > "
  rawInput <- getLine
  let pilihan = map toUpper (filter (/= ' ') rawInput)
  prosesMenu pilihan

prosesMenu :: String -> IO ()
prosesMenu "1" = menuMataUang    >> mainMenu
prosesMenu "2" = menuPanjang     >> mainMenu
prosesMenu "3" = menuSuhu        >> mainMenu
prosesMenu "4" = menuBerat       >> mainMenu
prosesMenu "5" = menuVolume      >> mainMenu
prosesMenu "6" = menuDataDigital >> mainMenu
prosesMenu "7" = menuBatch       >> mainMenu
prosesMenu "K" = do
  putStrLn "\n  Terima kasih! Program selesai."
  exitSuccess
prosesMenu _   = do
  putStrLn "\n  [!] Pilihan tidak valid. Silakan coba lagi."
  mainMenu

-- ----------------------------------------------------------------
-- KONVERSI BATCH - HIGHER-ORDER FUNCTION 
-- batchKonversi menerima fungsi sebagai argumen (HOF),
-- lalu mengaplikasikannya ke seluruh list menggunakan map.
-- ----------------------------------------------------------------

batchKonversi :: FungsiKonversi -> [Double] -> [Double]
batchKonversi f xs = map f xs

menuBatch :: IO ()
menuBatch = do
  putStrLn "\n=========== KONVERSI BATCH ==========="
  putStrLn "  Masukkan beberapa nilai (pisah spasi)"
  putStrLn "  Contoh: 1 5 10 100 1000 atau berapapun nilai yang ingin dikonversi."
  putStr "  Nilai > "
  inputNilai <- getLine
  let listNilai = map read (words inputNilai) :: [Double]
  putStrLn "\n  Pilih jenis konversi batch:"
  putStrLn "  [1] USD   -> IDR"
  putStrLn "  [2] EUR   -> IDR"
  putStrLn "  [3] SGD   -> IDR"
  putStrLn "  [4] KM    -> Meter"
  putStrLn "  [5] Meter -> KM"
  putStrLn "  [6] C     -> Fahrenheit"
  putStrLn "  [7] C     -> Kelvin"
  putStrLn "  [8] KG    -> Gram"
  putStr "  Pilih > "
  rawPil <- getLine
  let pil = map toUpper (filter (/= ' ') rawPil)
  case pil of
    "1" -> jalankanBatch listNilai usdKeIdr        "USD"   "IDR"
    "2" -> jalankanBatch listNilai eurKeIdr        "EUR"   "IDR"
    "3" -> jalankanBatch listNilai sgdKeIdr        "SGD"   "IDR"
    "4" -> jalankanBatch listNilai kmKeMeter       "KM"    "Meter"
    "5" -> jalankanBatch listNilai meterKeKm       "Meter" "KM"
    "6" -> jalankanBatch listNilai celsiusKeF      "C"     "Fahrenheit"
    "7" -> jalankanBatch listNilai celsiusKeKelvin "C"     "Kelvin"
    "8" -> jalankanBatch listNilai kgKeGram        "KG"    "Gram"
    _   -> putStrLn "  [!] Pilihan konversi tidak valid."

jalankanBatch :: [Double] -> FungsiKonversi -> String -> String -> IO ()
jalankanBatch listNilai f satuanDari satuanKe = do
  let hasil = batchKonversi f listNilai
  tampilkanHasilBatch listNilai hasil satuanDari satuanKe

tampilkanHasilBatch :: [Double] -> [Double] -> String -> String -> IO ()
tampilkanHasilBatch inputs outputs satuanDari satuanKe = do
  putStrLn $ "\n  --- Batch: " ++ satuanDari ++ " -> " ++ satuanKe ++ " ---"
  mapM_ cetakBaris (zip inputs outputs)
  putStrLn "  ------------------------------------"
  where
    cetakBaris (i, o) =
      putStrLn $ "  " ++ show i ++ " " ++ satuanDari
                 ++ "  =  " ++ show o ++ " " ++ satuanKe

-- FUNGSI KONVERSI (Placeholder - akan diganti saat integrasi)

usdKeIdr :: FungsiKonversi
usdKeIdr x = x * 16200.0

eurKeIdr :: FungsiKonversi
eurKeIdr x = x * 17500.0

sgdKeIdr :: FungsiKonversi
sgdKeIdr x = x * 12100.0

kmKeMeter :: FungsiKonversi
kmKeMeter x = x * 1000.0

meterKeKm :: FungsiKonversi
meterKeKm x = x / 1000.0

celsiusKeF :: FungsiKonversi
celsiusKeF x = (x * 9 / 5) + 32

celsiusKeKelvin :: FungsiKonversi
celsiusKeKelvin x = x + 273.15

kgKeGram :: FungsiKonversi
kgKeGram x = x * 1000.0


-- STUB MENU (akan diisi masing-masing PIC saat integrasi)

menuMataUang :: IO ()
menuMataUang = putStrLn "\n  [STUB] Modul Mata Uang    - PIC: Faris  (Tiket 3)"

menuPanjang :: IO ()
menuPanjang = putStrLn "\n  [STUB] Modul Panjang      - PIC: Daffa  (Tiket 4)"

menuSuhu :: IO ()
menuSuhu = putStrLn "\n  [STUB] Modul Suhu"

menuBerat :: IO ()
menuBerat = putStrLn "\n  [STUB] Modul Berat"

menuVolume :: IO ()
menuVolume = putStrLn "\n  [STUB] Modul Volume"

menuDataDigital :: IO ()
menuDataDigital = putStrLn "\n  [STUB] Modul Data Digital"