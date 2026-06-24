# Konverter Satuan & Mata Uang

Project Akhir Mata Kuliah Pemrograman Deklaratif - Kelompok 4.

Aplikasi *Command Line Interface* (CLI) interaktif yang dibangun menggunakan bahasa **Haskell**. Aplikasi ini dirancang untuk melakukan konversi mata uang secara *real-time* dengan menarik data langsung dari API eksternal.

## Fitur Utama

- **Konversi Real-Time:** Menarik data kurs mata uang global terkini menggunakan [Frankfurter API](https://www.frankfurter.app/).
- **Menu Interaktif:** Navigasi CLI yang berjalan terus menerus (*loop*) menggunakan konsep **Rekursi** dan I/O Monad.
- **Validasi Input:** Mengonversi input *user* secara otomatis (*case-insensitive*) memanfaatkan *Higher-Order Function* (`map`).
- **Data Parsing:** Parsing JSON otomatis ke dalam tipe data khusus (*Custom Types*) menggunakan `Data.Aeson`.

## Prasyarat (Prerequisites)

Sebelum menjalankan aplikasi ini, pastikan sistem sudah terinstal:
- **GHC** (Glasgow Haskell Compiler)
- **Cabal** (Haskell Build Tool)

*Rekomendasi: Gunakan [GHCup](https://www.haskell.org/ghcup/) untuk menginstal keduanya dengan mudah.*

## Cara Menjalankan Program

Repositori ini sudah distandarisasi sebagai **Cabal Project**, sehingga tidak perlu menginstal *library* secara manual. Cabal akan mengurus semua dependensi (seperti `aeson`, `http-conduit`, dan `containers`) di ruang yang terisolasi.

1. *Clone* repositori ini ke komputer lokal:
```bash
   git clone https://github.com/noireveil/decl-currency-converter.git
   cd decl-currency-converter
```

2. Pindah ke *branch* API integrasi:

```bash
   git checkout feat/frankfurter-api
```

3. Jalankan aplikasi menggunakan Cabal:

```bash
   cabal run
```

## Struktur Branch

* `main` : *Branch* utama yang berisi versi program menggunakan *fixed data* (data statis).
* `feat/frankfurter-api` : *Branch* eksperimental/lanjutan yang mengimplementasikan HTTP Request ke API Frankfurter untuk data kurs yang aktual.