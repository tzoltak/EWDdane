#' @title Pobieranie danych o uczniach.
#' @description
#' Funkcja pobiera z bazy dane o uczniach, których nie zwraca funkcja \code{pobierz_czesc_egzaminu()} z pakietu ZPD: informacje o klasie, numerze uczni i jego wieku (w miesiącach).
#' @param idTestow wektor liczbowy zawierający wartości \code{id_testu} testów, które rozwiązywali uczniowie, których mają dotyczyć zwracane dane
#' @param zrodloDanychODBC opcjonalnie nazwa źródła danych ODBC, dającego dostęp do bazy (domyślnie "EWD")
#' @details
#' Wszystkie \code{id_testu} podane w argumencie \code{idTestow} muszą być powiązane z tym samym egzaminem.
#' Wiek zwracany jest w miesiącach. Jeśli testy były pisane w różnych dniach, zwrócony zostanie wiek wyliczony w odniesieniu do daty testu, który został przeprowadzony najwcześniej.
#' @return data frame
#' @import RODBCext
#' @export
pobierz_dane_uczniow <- function(idTestow, zrodloDanychODBC="EWD"){
  stopifnot(is.numeric(idTestow), length(idTestow) > 0,
            is.character(zrodloDanychODBC), length(zrodloDanychODBC) == 1
  )
  try(suppressWarnings(Sys.setlocale("LC_ALL", "pl_PL.UTF-8")))

  P = odbcConnect(zrodloDanychODBC)

  zapytanie = paste0( "SELECT DISTINCT AR.rodzaj_egzaminu, EXTRACT(YEAR FROM AR.data_egzaminu)
                      FROM testy AS T JOIN arkusze AS AR using(arkusz)
                      WHERE id_testu IN (", paste0(rep("?", length(idTestow)), collapse=", "), ") AND ewd = TRUE")
  tryCatch({
      ret =  sqlExecute(P, zapytanie, fetch = TRUE, stringsAsFactors = FALSE, data=as.list(idTestow))
      odbcClose(P)
    },
    error=function(e) {
      odbcClose(P)
      stop(e)
    }
  )

  if(nrow(ret) > 1 ){
    stop("Podane testy powiązane są z różnymi egzaminami. Czy na pewno podano poprawne id testów?")
  }

  P = odbcConnect(zrodloDanychODBC)
  zapytanie = paste0( "SELECT DISTINCT ob.id_obserwacji, tob.klasa, tob.kod_u,
                      12*(EXTRACT(YEAR FROM t.pierwszy_egz) - EXTRACT(YEAR FROM ob.data_ur)) + (EXTRACT(MONTH FROM t.pierwszy_egz) - EXTRACT(MONTH FROM ob.data_ur)) AS wiek
                      FROM obserwacje AS ob
                        JOIN testy_obserwacje AS tob USING (id_obserwacji)
                        JOIN (SELECT
                                id_testu,
                                ( SELECT min(ar.data_egzaminu) AS pierwszy_egz
                                  FROM testy AS t JOIN arkusze AS ar USING (arkusz)
                                  WHERE id_testu IN (", paste0(rep("?", length(idTestow)), collapse=", "), ") AND ewd = TRUE
                                )
                              FROM testy
                              WHERE ewd = TRUE AND id_testu IN (", paste0(rep("?", length(idTestow)), collapse=", "), ")
                              ) AS t USING (id_testu)")
  tryCatch({
      ret =  sqlExecute(P, zapytanie, fetch = TRUE,  data=c(as.list(idTestow), as.list(idTestow)))
      odbcClose(P)
    },
    error=function(e) {
      odbcClose(P)
      stop(e)
    }
  )

  if (any(duplicated(ret$id_obserwacji))) {
    stop("Łączeniu informacji dot. różnych testów doprowadziło do powtórzeń id_obserwacji:\n  ", paste0(ret$id_obserwacji[duplicated(ret$id_obserwacji)], collapse=",\n  "), ".")
  }

  return(ret)
}
