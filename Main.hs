module Main where

import System.Exit (exitSuccess)
import IOUtils     (getInputRaw, getInputClean)
import Finansial   (menuMataUang)

-- TODO: Uncomment saat modul lain selesai
-- import Panjang

-- | Alias untuk fungsi konversi (Higher-Order Function)
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
    pilihan <- getInputClean "  Pilih > "
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

-- | Konversi Batch (Eta-reduced map)
batchKonversi :: FungsiKonversi -> [Double] -> [Double]
batchKonversi = map

menuBatch :: IO ()
menuBatch = do
    putStrLn "\n=========== KONVERSI BATCH ==========="
    putStrLn "  Masukkan beberapa nilai (pisah spasi)"
    putStrLn "  Contoh: 1 5 10 100 1000 atau berapapun nilai yang ingin dikonversi."
    inputNilai <- getInputRaw "  Nilai > "
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
    pil <- getInputClean "  Pilih > "
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