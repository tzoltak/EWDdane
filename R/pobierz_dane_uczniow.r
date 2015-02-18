#' @title Pobieranie danych o uczniach.
#' @description
#' Funkcja pobiera z bazy dane o uczniach, których nie zwraca funkcja
#' \code{pobierz_czesc_egzaminu()} z pakietu ZPD: informacje o klasie, numerze ucznia
#' i jego wieku (w miesiącach).
#' @param idTestow wektor liczbowy zawierający wartości \code{id_testu} testów, które
#' rozwiązywali uczniowie, których mają dotyczyć zwracane dane
#' @param zrodloDanychODBC opcjonalnie nazwa źródła danych ODBC, dającego dostęp do bazy
#' (domyślnie "EWD")
#' @details
#' Wszystkie \code{id_testu} podane w argumencie \code{idTestow} muszą być powiązane
#' z tym samym egzaminem. Wiek zwracany jest w miesiącach. Jeśli testy były pisane
#' w różnych dniach, zwrócony zostanie wiek wyliczony w odniesieniu do daty testu, który
#' został przeprowadzony najwcześniej.
#' @return data frame
#' @export
pobierz_dane_uczniow = function(idTestow, zrodloDanychODBC="EWD") {
  stopifnot(is.numeric(idTestow), length(idTestow) > 0,
            is.character(zrodloDanychODBC), length(zrodloDanychODBC) == 1
  )
  stop("Funkcja wymaga przepisania")
#   try(suppressWarnings(Sys.setlocale("LC_ALL", "pl_PL.UTF-8")))
#
#   zapytanie = paste0( "SELECT DISTINCT AR.rodzaj_egzaminu, EXTRACT(YEAR FROM AR.data_egzaminu)
#                       FROM testy AS T JOIN arkusze AS AR using(arkusz)
#                       WHERE id_testu IN (", paste0(rep("?", length(idTestow)), collapse=", "), ") AND ewd = TRUE")
#   tryCatch({
#       P = odbcConnect(zrodloDanychODBC)
#       ret =  sqlExecute(P, zapytanie, fetch = TRUE, stringsAsFactors = FALSE, data=as.list(idTestow))
#     },
#     error = stop,
#     finally = odbcClose(P)
#   )
#
#   if(nrow(ret) > 1 ){
#     stop("Podane testy powiązane są z różnymi egzaminami. Czy na pewno podano poprawne id testów?")
#   }
#
#   zapTest1 = "SELECT has_table_privilege('dane_osobowe.obserwacje', 'select')"
#   zapTest2 = "SELECT has_table_privilege('testy_obserwacje', 'select')"
#   do_obs =0
#   do_testy = 0
#   tryCatch({
#       P = odbcConnect(zrodloDanychODBC)
#       do_obs = sqlExecute(P, zapTest1, fetch = TRUE)
#       do_testy = sqlExecute(P, zapTest1, fetch = TRUE)
#     },
#     error = stop,
#     finally = odbcClose(P)
#   )
#
#   if(do_obs!=1 | do_testy!=1){
#     stop("Brak dostępu do danych poufnych.")
#   }
#
#   zapytanie = paste0( "SELECT DISTINCT ob.id_obserwacji, tob.klasa, tob.kod_u,
#                       12*(EXTRACT(YEAR FROM t.pierwszy_egz) - EXTRACT(YEAR FROM ob.data_ur)) + (EXTRACT(MONTH FROM t.pierwszy_egz) - EXTRACT(MONTH FROM ob.data_ur)) AS wiek
#                       FROM dane_osobowe.obserwacje AS ob
#                         JOIN dane_osobowe.testy_obserwacje AS tob USING (id_obserwacji)
#                         JOIN (SELECT
#                                 id_testu,
#                                 ( SELECT min(ar.data_egzaminu) AS pierwszy_egz
#                                   FROM testy AS t JOIN arkusze AS ar USING (arkusz)
#                                   WHERE id_testu IN (", paste0(rep("?", length(idTestow)), collapse=", "), ") AND ewd = TRUE
#                                 )
#                               FROM testy
#                               WHERE ewd = TRUE AND id_testu IN (", paste0(rep("?", length(idTestow)), collapse=", "), ")
#                               ) AS t USING (id_testu)")
#   tryCatch({
#       P = odbcConnect(zrodloDanychODBC)
#       ret = sqlExecute(P, zapytanie, fetch = TRUE,  data=c(as.list(idTestow), as.list(idTestow)))
#     },
#     error = stop,
#     finally = odbcClose(P)
#   )
#   if (any(duplicated(ret$id_obserwacji))) {
#     warning("Łączeniu informacji dot. różnych testów doprowadziło do powtórzeń id_obserwacji:\n  ",
#             paste0(ret$id_obserwacji[duplicated(ret$id_obserwacji)], collapse=",\n  "),
#             ".\nTam, gdzie problem polegał na konflikcie braku informacji z występowaniem informacji, został on jednak rozwiązany.")
#     duplikaty = with(ret, {ret$id_obserwacji[duplicated(ret$id_obserwacji)]})
#     bezDupl   = subset(ret, !(ret$id_obserwacji %in% duplikaty))
#     duplikaty = subset(ret,   ret$id_obserwacji %in% duplikaty )
#     duplikaty = ddply(duplikaty, ~id_obserwacji,
#                       function(x) {
#                         klasaINumer = rowSums(!is.na(x[, c("klasa", "kod_u")]))
#                         tylkoJedenWierszBezBD = (sum(klasaINumer == 2) == 1) |
#                           ( (max(klasaINumer) == 1) & (sum(klasaINumer == 1) == 1) )
#                         resztaWPorzadku = unique(x[, !(names(x) %in% c("id_obserwacji", "klasa", "kod_u")), drop=FALSE])
#                         resztaWPorzadku = nrow(resztaWPorzadku) == 1
#                         if (tylkoJedenWierszBezBD & resztaWPorzadku) {
#                           return(x[which.max(klasaINumer), ])
#                         } else {
#                           return(x)
#                         }
#                       })
#     ret = rbind(bezDupl, duplikaty)
#   }
#   return(ret)
}
