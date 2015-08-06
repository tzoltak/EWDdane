#' @title Agregowanie wskaźników EWD
#' @description
#' Funkcja agreguje wskaźniki EWD.
#' @param dane ramka danych zwracana przez funkcję \link{pobierz_wartosci_wskaznikow_ewd}.
#' @param poziom ciąg znaków definiujący poziom agregacji: "gmina", "powiat" lub
#' "województwo", albo \code{NULL}, jeśli podany został parametr 'funkcjaGrupujaca'
#' @param grupujPoLatach wartość logiczna (\code{TRUE} lub \code{FALSE})
#' wskazująca, czy przy wyróżnianiu grup oprócz TERYTu ma być uwzględniona
#' również zmienna opisująca okres, dla którego wyliczony został wskaźnik
#' @param funkcjaGrupujaca NULL lub fukcja przyjmująca jako pierwszy parametr
#' TERYT szkoły (w postaci wektora liczb całkowitych) i zwracająca jednoelementowy
#' data frame (lub listę), który opisuje przydział szkół do grup
#' @param tylkoWyswietlane wartość logiczna (\code{TRUE/FALSE/NA}) opisująca,
#' czy przy wyliczaniu agregatów mają zostać uwzględnione tylko te szkoły,
#' których elipsy są pokazywane na stronie - p. sekcja Details
#' @param paramGrupujaca opcjonalnie lista dodatkowych parametrów, które zostaną
#' przekazane funkcji grupującej
#' @details
#' Zachowanie funkcji w zależności od wartości parametru \code{tylkoWyswietlane}:
#' \itemize{
#'   \item{Jeśli \code{tylkoWyswietlane=TRUE} przy wyliczaniu agregatów
#'         uwzględniane zostaną tylko te szkoły, których elipsy są wyświetlane
#'         na stronie.}
#'   \item{Jeśli \code{tylkoWyswietlane=FALSE} przy wyliczaniu agregatów
#'         uwzględniane zostaną wszystkie szkoły}
#'   \item{Domyślnie, tj. gdy \code{tylkoWyswietlane=NA}, przy wyliczaniu
#'         agregatów uwzględniane zostaną wszystkie szkoły, (jak wtedy, gdy
#'         \code{tylkoWyswietlane=TRUE}) z tym, że jeśli w jakiejś grupie nie ma
#'         żadnej szkoły, której elipsa byłaby prezentowana, to wartość
#'         wskaźnika zostanie w niej zmieniona na brak danych.}
#' }
#' Oznacza to, że dla poprawnego działania domyślnego wywołwania funkcji
#' \code{agreguj_wskazniki_ewd} potrzebne jest pobranie wcześniej danych
#' o wartościach wskaźników wywołaniem funkcji
#' \code{\link{pobierz_wartosci_wskaznikow_ewd}} z inną niż domyślna wartością
#' parametru \code{tylkoWyswietlane} - p. przykład użycia.
#' @return data frame
#' @examples
#' \dontrun{
#' dane = pobierz_wartosci_wskaznikow_ewd("T", 2013:2014, tylkoWyswietlane = FALSE) %>%
#'   agreguj_wskazniki_ewd("powiat")
#' }
#' @import dplyr
#' @import ZPD
#' @export
agreguj_wskazniki_ewd <- function(dane, poziom = NULL, grupujPoLatach = TRUE,
                                  funkcjaGrupujaca = NULL, tylkoWyswietlane = NA,
                                  paramGrupujaca = list()) {
  stopifnot(is.data.frame(dane),
            !is.null(poziom) | !is.null(funkcjaGrupujaca),
            is.character(poziom) | is.null(poziom),
            is.logical(grupujPoLatach), length(grupujPoLatach) == 1,
            is.function(funkcjaGrupujaca) | is.null(funkcjaGrupujaca),
            is.logical(tylkoWyswietlane), length(tylkoWyswietlane) == 1,
            is.list(paramGrupujaca))
  if (!is.null(poziom)) stopifnot(poziom %in% c("gmina", "powiat", "województwo"))
  stopifnot(grupujPoLatach %in% c(TRUE, FALSE))

  if ( "teryt_szkoly" %in% names(dane) ) {
    czyOpisowe = FALSE
  } else if ("TERYT gminy" %in% names(dane) ) {
    czyOpisowe = TRUE
    names(dane) = konwertuj_nazwy_na_opisowe(names(dane), TRUE)
  } else {
    stop("Funkcja nie zawiera zmiennej opisującej teryt")
  }
  if (!is.null(poziom)) {
    funkcjaGrupujaca = przygotuj_funkcje_grupujaca_teryt(poziom)
  }
  if (grupujPoLatach) {
    rok_do = "rok_do"
  } else {
    rok_do = NULL
  }

  grupowanie = do.call(funkcjaGrupujaca, append(list(dane$teryt_szkoly),
                                                paramGrupujaca))
  if ( !is.data.frame(grupowanie) | ncol(grupowanie) != 1 |
        nrow(grupowanie) != nrow(dane) ) {
    stop("Niepoprawny format danych zwracany przez funkcję grupującą.")
  }
  nazwaGrupowania = names(grupowanie)
  names(dane) = enc2native(names(dane))  # jako że select() nie radzi sobie z UTFem
  dane = cbind(dane %>% select_(~ -teryt_szkoly), grupowanie)

  dane = dane[, grepl("^(id_szkoly|rok_do)$|^teryt|^(ewd|lu_ewd|wyswietlaj|srednia)[_ ]",
                      tolower(names(dane)))]

  zmienne = c("id_szkoly", "rok_do", nazwaGrupowania)
  maskaZmienne = "^(ewd|lu_ewd|wyswietlaj|srednia)[_ ]"
  dane = melt(dane[, c(zmienne,
                       names(dane)[grepl(maskaZmienne, names(dane))])],
              id = zmienne)
  dane = cbind(dane,
               wskaznik = gsub(maskaZmienne, "", dane$variable))
  dane$variable = gsub(paste0(maskaZmienne, ".*$"), "\\1",
                       dane$variable)

  formulaTemp = as.formula(paste0(paste0(zmienne, collapse = "+"),
                                  " + wskaznik ~ variable"))
  dane = dcast(dane, formulaTemp, value.var = "value") %>%
    group_by_(.dots = list(rok_do, nazwaGrupowania, "wskaznik"))

  # Gdy tylkoWyswietlane to TRUE, odfiltruj niewyświetlane.
  if (tylkoWyswietlane %in% TRUE & "wyswietlaj" %in% names(dane)) {
    dane = dane %>% filter_(~ wyswietlaj == 1)
  } else if (tylkoWyswietlane %in% TRUE) {
    message("W danych brak informacji o tym, dla których szkół wyświetlane są elipsy. ",
            "Aby mieć pewność, że szkoły, dla których elipsy nie są wyświetlane, ",
            "nie zostały uwzględnione, sprawdź, czy w wywołaniu funkcji ",
            "pobierz_wartosci_wskaznikow_ewd(), które zwróciło dane ",
            "przekazywane teraz do agregacji, argument 'tylkoWyswietlane' ",
            "miał przypisaną wartość TRUE ",
            "('pobierz_wartosci_wskaznikow_ewd(..., tylkoWyswietlane = TRUE)').")
  } else if (is.na(tylkoWyswietlane) & !("wyswietlaj" %in% names(dane)) ) {
    stop("W danych brak informacji o tym, dla których szkół wyświetlane są elipsy. ",
         "Jest ona jednak niezbędna, aby móc dokonać agregacji zgodnie z procedurą, ",
         "którą zakłada wywołanie funkcji agreguj_wskazniki_ewd() z parametrem ",
         "'tylkoWyswietlane' równym NA (jest to też domyślna wartość tego parametru).",
         "\n\nJeśli chcesz przeprowadzić agregację zgodnie z domyślną procedurą ",
         "('tylkoWyswietlane = NA'), musisz ściągając dane do agregacji wywołać ",
         "funkcję pobierz_wartosci_wskaznikow_ewd() z argumentem ",
         "'tylkoWyswietlane' ustawionym na FALSE ",
         "('pobierz_wartosci_wskaznikow_ewd(..., tylkoWyswietlane = FALSE)').")
  }
  if (!("wyswietlaj" %in% names(dane))) dane = mutate(dane, wyswietlaj = NA)

  dane = dane %>%
    summarise_(.dots = list(ewd = ~ weighted.mean(ewd, lu_ewd, na.rm = TRUE),
                            srednia = ~ weighted.mean(srednia, lu_ewd, na.rm = TRUE),
                            lu_ewd = ~ sum(lu_ewd, na.rm = TRUE),
                            wyswietlaj = ~ any(wyswietlaj == 1) ))
  if (is.na(tylkoWyswietlane) & "wyswietlaj" %in% names(dane)) {
    dane$ewd[!dane$wyswietlaj] = NA
    dane$srednia[!dane$wyswietlaj] = NA
  }
  dane = select_(dane, ~ -wyswietlaj) %>%
    melt(c(rok_do, nazwaGrupowania, "wskaznik")) %>%
    dcast(as.formula(paste0(paste0(c(rok_do, nazwaGrupowania), collapse = "+"),
                            " ~ wskaznik + variable")))

  if (czyOpisowe) {
    names(dane) = konwertuj_nazwy_na_opisowe(names(dane))
    names(dane) = gsub("_", " ", names(dane))
  }

  return(as.data.frame(dane))
}
#' @title Przygotowanie funkcji grupującej w JST na podstawie TERYTu
#' @description
#' Funkcja przygotowuje funkcję grupującą. Więcej szczegółów
#' w \link{agreguj_wskazniki_ewd}
#' @param poziom ciąg znaków definiujący poziom agregacji: "gmina", "powiat" lub
#' "województwo"
#' @return funkcja
przygotuj_funkcje_grupujaca_teryt <- function(poziom){
  stopifnot(poziom %in% c("gmina", "powiat", "województwo"))
  fun = switch(poziom,
               gmina = function(teryt){
                 return(data.frame(teryt_gminy = teryt))
               },
               powiat = function(teryt){
                 return(data.frame(teryt_powiatu = round(teryt / 100) * 100))
               },
               wojewodztwo = function(teryt){
                 return(data.frame("teryt_województwa" = round(teryt / 10^4) * 10^4))
               }
  )
  return(fun)
}
#' @title Kowersja nazw na opisowe
#' @description
#' Funkcja konwertuje nazwy kolumn ramki danych zwracanych przez funkcję
#' \code{\link{pobierz_wartosci_wskaznikow_ewd}}.
#' @param nazwyKolumn nazwy kolumn do skonwertowania
#' @param revert wartość logiczna. Jeżeli revert wynosi TRUE to nazwy opisowe są
#' zmienane na zwykłe. Domyślna wartość FALSE powoduje zmianę nazw zwykłych na
#' nazwy opisowe.
#' @return wektor ciągów znaków
konwertuj_nazwy_na_opisowe <- function(nazwyKolumn, revert = FALSE){
  klucz = rbind(
    c("id_szkoly"    , "id szkoły w bazie EWD"),
    c("artystyczna"  , "typ szkoły artystycznej"),
    c("typ_szkoly"   , "typ szkoły"),
    c("dla_doroslych", "szkoła dla dorosłych"),
    c("specjalna"    , "szkoła specjalna"),
    c("przyszpitalna", "szkoła przyszpitalna"),
    c("id_szkoly_oke", "kod egzaminacyjny szkoły"),
    c("nazwa_szkoly" , "nazwa"),
    c("miejscowosc"  , "miejscowość"),
    c("pna"          , "kod pocztowy"),
    c("wielkosc_miejscowosci", "wielkość miejscowości"),
    c("teryt_szkoly" , "TERYT gminy"),
    c("rodzaj_gminy" , "rodzaj gminy"),
    c("rok_do"       , "ostatni rok okresu obejmowanego przez wskaźnik"),
    c("^pomin$"      , "czy szkoła pomijana na stronie"),
    c("wyswietlaj"   , "czy elipsa wyświetlana -"),
    c("dg_pu_"       , "dolna granica przedz. ufności dla" ),
    c("gg_pu_"       , "górna granica przedz. ufności dla" ),
    c("srednia"      , "śr. wyników egzaminów"),
    c("_lu_"         , " liczba uczniów "),
    c("lu_"          , "liczba uczniów "),
    c("_trend_EWD"   , " trend EWD"),
    c("ewd"          , "EWD"))
  klucz = data.frame(klucz)
  if (!revert) {
    colnames(klucz) = c("orginalne", "nowe")
  } else{
    colnames(klucz) = c("nowe", "orginalne")
    klucz$nowe = sub("[:^:]|[:$:]", "", klucz$nowe)
  }

  for (k in seq_len(nrow(klucz))) {
    nazwyKolumn = sub(klucz$orginalne[k], klucz$nowe[k], nazwyKolumn)
  }
  return(nazwyKolumn)
}
