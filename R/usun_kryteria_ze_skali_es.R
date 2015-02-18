#' @title Usuwanie zbędnych kryteriów i pseudokryteriów ze skali
#' @description
#' Funkcja przygotowuje obiekt do modyfikacji skali, który nie zawiera zbędnych kryteriów
#' i pseudokryteriów.
#' @param skalowanie lista zawierająca obiekt usunieteKryteria - wektor nazw kryteriów do
#' usunięcia.
#' @param idSkali id skali, z której usuwane są kryteria.
#' @param zrodloDanychODBC opcjonalnie nazwa źródła danych ODBC, dającego dostęp do bazy
#' (domyślnie "EWD")
#' @return Funkcja zwraca ramkę danych, która podana do funkcji
#' \code{\link[ZPDzapis]{edytuj_skale}} usunie z niej zbędne (pseudo)kryteria.
#' @export
usun_kryteria_ze_skali_es = function(skalowanie, idSkali, zrodloDanychODBC="EWD") {
  stop("Funkcja wymaga przepisania")

#   kryteriaDoUsuniecia = as.numeric(gsub("k_", "", skalowanie$usunieteKryteria[grepl("^k_", skalowanie$usunieteKryteria)] ))
#   pseudoDoUsuniecia = as.numeric(gsub("p_", "", skalowanie$usunieteKryteria[grepl("^p_", skalowanie$usunieteKryteria)] ))
#   zapytanie = "select id_kryterium as numer, 'K' as rodzaj from skale
#   join skale_elementy using(id_skali)
#   join kryteria_oceny using(id_kryterium)
#   where id_skali = ?
#   union
#   select id_pseudokryterium as numer, 'P' as rodzaj from skale
#   join skale_elementy using(id_skali)
#   join pseudokryteria_oceny using(id_pseudokryterium)
#   where id_skali = ?
#   order by numer"
#
#   tryCatch({
#     P = odbcConnect(as.character(zrodloDanychODBC))
#     # pobranie danych o pseudokryterium
#     wynik = sqlExecute(P, zapytanie, data =data.frame(idSkali, idSkali)  , fetch = TRUE, stringsAsFactors = FALSE)
#     odbcClose(P)
#   },
#   error=function(e) {
#     odbcClose(P)
#     stop(e)
#   }
#   )
#
#   kryteriaDoBazy = wynik$numer[!wynik$numer %in% kryteriaDoUsuniecia & wynik$rodzaj == "K"]
#   pseudoDoBazy = wynik$numer[!wynik$numer %in% pseudoDoUsuniecia & wynik$rodzaj == "P"]
#
#   ret = data.frame(opis = character(0), id_skrotu = numeric(0),
#                    id_pseudokryterium = numeric(0), id_kryterium = numeric(0))
#
#   kryteria = wynik$numer[wynik$rodzaj=="K"]
#   ret = rbind.fill(ret, data.frame(id_kryterium = kryteriaDoBazy))
#
#   pseudok = wynik$numer[wynik$rodzaj=="P"]
#   ret = rbind.fill(ret, data.frame(id_pseudokryterium = pseudoDoBazy))
#   return(ret)
}
