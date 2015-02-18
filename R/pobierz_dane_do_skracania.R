#' @title Pobieranie danych przefiltrowanych pod kątem wyliczania skrótów skal ocen
#' @description
#' Funkcja pobiera dane z określonej części egzaminu. Następnie z tych danych usuwani są
#' uczniowie, którzy przystępowali do egzaminu wielokrotnie lub uczęszczali do szkoły dla
#' dorosłych, specjalnych lub przyszpitalnych. Usunięto także uczniów, których odział był
#' oznaczony cyfrą.
#' @param rodzajEgzaminu rodzaj egzaminu
#' @param czescEgzaminu część egzaminu
#' @param rokEgzaminu rok egzaminu
#' @param idSkali numer skali
#' @param zrodloDanychODBC opcjonalnie nazwa źródła danych ODBC, dającego dostęp do bazy
#' (domyślnie "EWD")
#' @return data frame
#' @export
pobierz_dane_do_skracania = function(rodzajEgzaminu, czescEgzaminu, rokEgzaminu,
                                      idSkali=NULL, zrodloDanychODBC = "EWD") {
  stop("Funkcja wymaga przepisania")
#   dane = pobierz_czesc_egzaminu(rodzajEgzaminu, czescEgzaminu, rokEgzaminu, TRUE,
#                                 punktuj = TRUE, idSkali, skroc = TRUE,
#                                 zrodloDanychODBC)
#   id_testu = unique(dane$id_testu)
#
#   wielokrotnie = wielokrotnie_przystepujacy(rodzajEgzaminu, zrodloDanychODBC)
#   wielokrotnie = wielokrotnie[wielokrotnie$rok_min <= rokEgzaminu & wielokrotnie$rok_max >= rokEgzaminu, ]
#   dane = dane[!dane$id_obserwacji %in% wielokrotnie$id_obserwacji, ]
#
#   szkoly = pobierz_dane_szkol(rokEgzaminu, typySzkol = NULL, idOke = FALSE,
#                               daneAdresowe = FALSE, dolaczPaou = FALSE, zrodloDanychODBC)
#   zleSzkoly = szkoly$id_szkoly[szkoly$dla_doroslych==1 | szkoly$specjalna==1 | szkoly$przyszpitalna==1]
#   dane = dane[!dane$id_szkoly %in% zleSzkoly, ]
#
#   uczniowie = pobierz_dane_uczniow(id_testu, zrodloDanychODBC)
#   uczniowie$klasa = sapply(uczniowie$klasa, as.character)
#
#   oddzialy = suppressWarnings(as.numeric(unique(uczniowie$klasa)))
#   zleOddzialy = oddzialy[!is.na(oddzialy)]
#   zliUczniowie = uczniowie$id_obserwacji[uczniowie$klasa %in% as.character(zleOddzialy)]
#   dane = dane[!dane$id_obserwacji %in% zliUczniowie, ]
#
#   return(dane)
}
