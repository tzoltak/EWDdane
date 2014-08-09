#' @title Uczniowie wielokrotnie przystępujący do egzaminu.
#' @description
#' Funkcja zwraca informacje o uczniach wielokrotnie przystępujących do egzaminu.
#' @param rodzajEgzaminu rodzaj egzaminu ("sprawdzian" / "egzamin gimnazjalny" / "matura")
#' @param zrodloDanychODBC opcjonalnie nazwa źródła danych ODBC, dającego dostęp do bazy (domyślnie "EWD")
#' @details
#' Wszystkie \code{id_testu} podane w argumencie \code{idTestow} muszą być powiązane z tym samym egzaminem.
#' Wiek zwracany jest w miesiącach. Jeśli testy były pisane w różnych dniach, zwrócony zostanie wiek wyliczony w odniesieniu do daty testu, który został przeprowadzony najwcześniej.
#' @return Data frame, który zawiera \code{id_obserwacji} oraz pierwszy i ostatni rok, w którym dany uczeń przystąpił do egzaminu.
#' W zwracanym obiekcie występują tylko ci uczniowie, którzy podchodzili do egzaminu wielokrotnie (tj. pierwszy rok przystąpienia nie jest dla nich równy ostatniemu rokowi przystąpienia).
#' @import RODBCext
#' @export
wielokrotnie_przystepujacy <- function(rodzajEgzaminu, zrodloDanychODBC="EWD"){
  stopifnot(is.character(rodzajEgzaminu), length(rodzajEgzaminu) == 1,
            is.character(zrodloDanychODBC), length(zrodloDanychODBC) == 1
  )
  stopifnot(rodzajEgzaminu %in% c("sprawdzian", "egzamin gimnazjalny", "matura"))
  try(suppressWarnings(Sys.setlocale("LC_ALL", "pl_PL.UTF-8")))

  P = odbcConnect(zrodloDanychODBC)

  zapytanie = paste0("SELECT * FROM
                        (SELECT id_obserwacji, rodzaj_egzaminu,
                          min(EXTRACT (YEAR FROM data_egzaminu)) AS rok_min,
                          max(EXTRACT (YEAR FROM data_egzaminu)) AS rok_max
                      	FROM testy_obserwacje
		                      JOIN testy USING (id_testu)
		                      JOIN arkusze USING (arkusz)
	                      WHERE rodzaj_egzaminu = ? AND ewd=TRUE
	                      GROUP BY id_obserwacji, rodzaj_egzaminu
                        ) AS status
                      WHERE rok_min != rok_max;")
  tryCatch({
      ret =  sqlExecute(P, zapytanie, fetch = TRUE, stringsAsFactors = FALSE, data=rodzajEgzaminu)
      odbcClose(P)
    },
    error=function(e) {
      odbcClose(P)
      stop(e)
    }
  )
  if (nrow(ret) == 0) message("Nie znaleziono żadnych wielokrotnie przystępujących.")

  return(ret)
}
