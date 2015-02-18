#' @title Zapisuje oszacowania umiejętności do bazy danych
#' @description
#' Funkcja zapisuje oszacowania umiejętności uczniów do bazy danych.
#' @param oszacowania ramka danych zawierająca oszacowania, błędy standardowe oraz id
#' testu lub testów poszczególnych uczniów.
#' @param skrotCzesci skrócona nazwa częsci egzaminu, np.: gh_p, gh.
#' @param rEAP rzetelność empiryczna
#' @param rodzajEgzaminu rodzaj egzaminu
#' @param rokEgzaminu rok egzaminu
#' @param nrSkalowania numer skalowania dla skali
#' @param idSkali id skali, z której usuwane są kryteria.
#' @param rodzajEstymacji ciąg znakowy opisujący rodzaj estymacji. Domyślnie przyjmuje
#' wartość 'EAP'.
#' @param zrodloDanychODBC opcjonalnie nazwa źródła danych ODBC, dającego dostęp do bazy
#' (domyślnie "EWD")
#' @param maskaTestowa wektor liczb określający, które wiersze ramki 'oszacowania' mają
#' być zapisane.
#' @return Funkcja zwraca ramkę danych, która podana do funkcji
#' \code{\link[ZPDzapis]{edytuj_skale}} usunie z niej zbędne (pseudo)kryteria.
#' @export
zapisz_oszacowania_umiejetnosci = function(oszacowania, skrotCzesci, rEAP,
                                           rodzajEgzaminu, rokEgzaminu, nrSkalowania,
                                           idSkali, rodzajEstymacji = 'EAP',
                                           zrodloDanychODBC = "EWD", maskaTestowa=NULL){

  stopifnot(is.list(oszacowania), is.character(skrotCzesci), is.numeric(rEAP),
            is.character(rodzajEgzaminu), is.numeric(rokEgzaminu),
            is.numeric(nrSkalowania), is.numeric(idSkali), is.character(rodzajEstymacji),
            is.character(zrodloDanychODBC), is.numeric(maskaTestowa))

  stop("Funkcja wymaga przepisania")
#   testy = which(grepl("^id_testu", names(oszacowania)))
#
#   if(length(testy)>1){
#     idTestow = as.vector(sapply(testy, function(x) { unique(oszacowania[[x]])}))
#
#     zapytanie = paste0("select distinct czesc_egzaminu from testy
#                        join arkusze using(arkusz)
#                        where id_testu in (", paste0(rep("?", length(idTestow)), collapse =",") ,")")
#
#     tryCatch({
#       P = odbcConnect(as.character(zrodloDanychODBC))
#       czesciEgzaminu = sqlExecute(P, zapytanie, data = data.frame(t(idTestow)), fetch = TRUE, stringsAsFactors = FALSE)[[1]]
#       odbcClose(P)
#     },
#     error=function(e) {
#       odbcClose(P)
#       stop(e)
#     }
#     )
#
#     if( sum(grepl("^id_testu_gh", names(oszacowania)[testy])) == length(testy) ){
#       nazwaEgz = "humanistyczna"
#     } else if ( sum(grepl("^id_testu_gm", names(oszacowania)[testy])) == length(testy) ) {
#       nazwaEgz = "matematyczno-przyrodnicza"
#     } else {
#       stop("Nie wszystkie nazwy testów są poprawne: ", names(oszacowania)[testy])
#     }
#
#     opisEgzaminu = paste(rodzajEgzaminu, nazwaEgz, rokEgzaminu, sep=";")
#
#     idTestu = suppressWarnings(stworz_test_z_wielu_czesci(rodzajEgzaminu, czesciEgzaminu, rokEgzaminu, czyEwd = TRUE, opis = opisEgzaminu, zrodloDanychODBC))
#   } else {
#     idTestu = oszacowania[[4]]
#   }
#
#   zapytanie = "INSERT INTO skalowania_obserwacje (id_testu, id_obserwacji, id_skali, skalowanie, estymacja,  wynik, bl_std, nr_pv)
#               VALUES (?, ?, ?, ?, ?, ?,  ?, -1)"
#
#   doWstawienia = data.frame(idTestu, oszacowania$id_obserwacji, idSkali, nrSkalowania, rodzajEstymacji,
#                             oszacowania[[skrotCzesci]]/rEAP,  oszacowania[[paste0(skrotCzesci,"_se")]]/rEAP)
#
#   if(!is.null(maskaTestowa)){
#     doWstawienia = doWstawienia[maskaTestowa, ]
#   }
#
#   tryCatch({
#     P = odbcConnect(as.character(zrodloDanychODBC))
#     odbcSetAutoCommit(P, FALSE)
#     czesciEgzaminu = sqlExecute(P, zapytanie, data = doWstawienia, fetch = TRUE, stringsAsFactors = FALSE)[[1]]
#     odbcEndTran(P, TRUE)
#     odbcClose(P)
#   },
#   error=function(e) {
#     odbcEndTran(P, FALSE)
#     odbcClose(P)
#     stop(e)
#   }
#   )
#   return(invisible(NULL))
}
