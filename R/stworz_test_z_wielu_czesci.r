#' @title Tworzenie testów będących złączeniem kilku części egzaminu.
#' @description
#' Funkcja tworzy nowy test ze wskazanych części wskazanego (poprzez rodzaj i rok) egzaminu
#' i wypełnia tablicę \code{dane_osobowe.testy_obserwacje} uczniami, którzy wystąpili
#' w przynajmniej jednej z części wskazanego egzaminu.
#' @param rodzajEgzaminu rodzaj egzaminu ("sprawdzian" / "egzamin gimnazjalny" / "matura")
#' @param czesciEgzaminu wektor części egzaminu, które mają zostać połączone
#' @param rokEgzaminu rok egzaminu
#' @param czyEwd czy dane mają pochodzić z testów EWD
#' @param opis opis nowetworzonego testu w tablic \code{testy}
#' @param zrodloDanychODBC opcjonalnie nazwa źródła danych ODBC, dającego dostęp do bazy (domyślnie "EWD")
#' @details
#' Tworzony test posiada datę, jak najstarsza z data części egzaminu, z których powstał
#' oraz \code{ewd} zgodne z testami, z których powstał.
#' 
#' Spośród kolumn tablicy \code{dane_osobowe.testy_obserwacje} kopiowana jest tylko kolumna
#' \code{id_szkoly}, natomiast kolumna \code{zrodlo} ustawiana jest na wartość \code{baza}.
#' Pozostałe kolumny (np. \code{dysleksja, klasa}, itp.) ustawiane są na \code{NULL}.
#' 
#' Jeśli w bazie istnieje już test o takim samym opisie, zrówcone zostanie id_testu tego testu
#' i jednocześnie wygenerowany zostanie warning informujący o tym, że nowy test nie został utworzony.
#' Uwaga! Nie gwarantuje to, że istniejący w bazie test składa się z identycznych testów składowych
#' - porównanie bazuje tylko na opisie testu, który jest ustawiany arbitralnie przez tworządego test.
#' @return id_testu utworzonego testu
#' @import RODBCext
#' @export
 stworz_test_z_wielu_czesci = function(rodzajEgzaminu, czesciEgzaminu, rokEgzaminu, czyEwd, opis, zrodloDanychODBC = "EWD"){
  stopifnot(
    is.character(rodzajEgzaminu), 
    length(rodzajEgzaminu) == 1,
    is.character(czesciEgzaminu),
    length(czesciEgzaminu) > 1,
    is.numeric(rokEgzaminu),
    length(rokEgzaminu) == 1,
    is.logical(czyEwd),
    length(czyEwd) == 1,
    is.character(opis),
    length(opis) == 1,
    opis[1] != '',
    is.character(zrodloDanychODBC), 
    length(zrodloDanychODBC) == 1
  )
  try(suppressWarnings(Sys.setlocale("LC_ALL", "pl_PL.UTF-8")))
  
  P = odbcConnect(zrodloDanychODBC)
  tryCatch(
    {
      czyJest = sqlExecute(P, "SELECT id_testu FROM testy WHERE opis = ?", opis, fetch = T)[, 1]
      if(length(czyJest) > 0){
        warning('w bazie istnieje już taki test')
        return(czyJest[1])
      }
      
      zapytanie = paste0("
        SELECT id_testu, data_egzaminu 
        FROM testy JOIN arkusze USING (arkusz)
        WHERE 
          rodzaj_egzaminu = ? 
          AND czesc_egzaminu IN (", 
            paste0(rep('?', length(czesciEgzaminu)), collapse = ', '),
          ")
          AND extract(year FROM data_egzaminu) = ?
          AND ewd = ?
        ORDER BY data")
      testy = sqlExecute(P, zapytanie, t(c(rodzajEgzaminu, czesciEgzaminu, rokEgzaminu, czyEwd)), T, stringsAsFactors = F)
      
      stopifnot(nrow(testy) > 0)
      
      odbcSetAutoCommit(P, FALSE)
      
      # Pobierz testy składowe
      idTestu = sqlExecute(P, "SELECT nextval('testy_id_testu_seq')", NULL, T)[1, 1]
      zapytanie = "
        INSERT INTO testy (id_testu, opis, data, ewd, arkusz)
        VALUES (?, ?, ?, ?, null)"
      sqlExecute(P, zapytanie, data.frame(idTestu, opis, testy$data_egzaminu[1], czyEwd))
      
      # Przygotuj zapytania źródłowe
      coalesce = function(testy, kolumna){
        paste0('t', seq_along(testy$id_testu), '.', kolumna, collapse = ', ')
      }
      zapFrom = paste0("
        FULL JOIN (
          SELECT id_obserwacji, id_szkoly, oke
          FROM dane_osobowe.testy_obserwacje
          WHERE id_testu = ?
        ) AS t", seq_along(testy$id_testu), " USING (id_obserwacji)",
        collapse = ''
      )
      zapFrom = sub('FULL JOIN', '', zapFrom)
      zapFrom = sub('USING [(]id_obserwacji[)]', '', zapFrom)
      
      # Wykonaj wstawianie
      zapytanie = paste0("
        INSERT INTO dane_osobowe.testy_obserwacje (id_obserwacji, id_testu, id_szkoly, zrodlo)
          SELECT id_obserwacji, ?, COALESCE(", coalesce(testy, 'id_szkoly'), "), 'baza'
          FROM ", zapFrom,
        collapse = ''
      )
      sqlExecute(P, zapytanie, t(c(idTestu, testy$id_testu)))
      
      odbcEndTran(P, TRUE)
      
      return(idTestu)
    },
    finally = function(){
      odbcClose(P)      
    }
  )
}
