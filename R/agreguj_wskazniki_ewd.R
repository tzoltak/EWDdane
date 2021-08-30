#' @title Agregowanie wskaznikow EWD
#' @description
#' Funkcja agreguje wskaźniki EWD.
#' @param dane ramka danych zwracana przez funkcję \link{pobierz_wartosci_wskaznikow_ewd}.
#' @param poziom ciąg znaków definiujący poziom agregacji: "gmina", "powiat" lub
#' "województwo", albo \code{NULL}, jeśli podany został parametr
#' \code{zmiennaGrupujaca}
#' @param grupujPoLatach wartość logiczna (\code{TRUE} lub \code{FALSE})
#' wskazująca, czy przy wyróżnianiu grup oprócz TERYTu ma być uwzględniona
#' również zmienna opisująca okres, dla którego obliczony został wskaźnik
#' @param zmiennaGrupujaca ciąg znaków - nazwa zmiennej grupującej
#' @param tylkoWyswietlane wartość logiczna (\code{TRUE/FALSE/NA}) opisująca,
#' czy przy obliczaniu agregatów mają zostać uwzględnione tylko te szkoły,
#' których elipsy są pokazywane na stronie - p. sekcja Details
#' @param pu wartość logiczna (\code{TRUE} lub \code{FALSE}) wskazująca, czy
#' funkcja ma zwrócić oszacowania granic przedziałów ufności dla obliczonych
#' średnich?
#' @param gammaDane poziom ufności dla jakiego zostały obliczone granice
#' przedziałów ufności podane w kolumnach argumentu \code{dane} - liczba
#' z przedziału (0;1) (ma zastosowanie tylko gdy \code{pu=TRUE})
#' @param gamma poziom ufności - liczba z przedziału (0;1) (ma zastosowanie
#' tylko gdy \code{pu=TRUE})
#' @details
#' Zachowanie funkcji w zależności od wartości parametru \code{tylkoWyswietlane}:
#' \itemize{
#'   \item{Jeśli \code{tylkoWyswietlane=TRUE} przy obliczaniu agregatów
#'         uwzględniane zostaną tylko te szkoły, których elipsy są wyświetlane
#'         na stronie.}
#'   \item{Jeśli \code{tylkoWyswietlane=FALSE} przy obliczaniu agregatów
#'         uwzględniane zostaną wszystkie szkoły}
#'   \item{Domyślnie, tj. gdy \code{tylkoWyswietlane=NA}, przy obliczaniu
#'         agregatów uwzględniane zostaną wszystkie szkoły, (jak wtedy, gdy
#'         \code{tylkoWyswietlane=TRUE}) z tym, że jeśli w jakiejś grupie nie ma
#'         żadnej szkoły, której elipsa byłaby prezentowana, to wartość
#'         wskaźnika zostanie w niej zmieniona na brak danych.}
#' }
#' Oznacza to, że dla poprawnego działania domyślnego wywołania funkcji
#' \code{agreguj_wskazniki_ewd} potrzebne jest pobranie wcześniej danych
#' o wartościach wskaźników wywołaniem funkcji
#' \code{\link{pobierz_wartosci_wskaznikow_ewd}} z inną niż domyślna wartością
#' parametru \code{tylkoWyswietlane} - p. przykład użycia.
#'
#' Uwaga, jeśli chce się uzyskać przedziały ufności dla obliczanych agregatów,
#' a funkcja  \code{\link{pobierz_wartosci_wskaznikow_ewd}} była wywoływana
#' z inną niż domyślna wartością parametru \code{gamma}, wartość tą należy przy
#' wywołaniu \code{agreguj_wskazniki_ewd} podać jako argument \code{gammaDane}.
#' @return data frame
#' @examples
#' \dontrun{
#' # agregacja wartości wskaźników dla techników z lat 2013-2014 do poziomu powiatu
#' agr = pobierz_wartosci_wskaznikow_ewd("T", 2013:2014, tylkoWyswietlane = FALSE) %>%
#'   agreguj_wskazniki_ewd("powiat")
#'
#' # j.w. ale agregacja po miejscowości podanej jako siedziba poczty
#' # ze zwróceniem granic przedziałów ufności dla agregatów
#' agr = pobierz_wartosci_wskaznikow_ewd("T", 2013:2014, tylkoWyswietlane = FALSE) %>%
#'   agreguj_wskazniki_ewd(zmiennaGrupujaca = "poczta", pu = TRUE)
#'
#' # przedziały ufności dla agregatów, jeśli pobierz_wartosci_wskaznikow_ewd()
#' # była wywołana z inną niż domyślna wartością parametru 'gamma'
#' # wywołując agreguj_wskazniki_ewd() trzeba użyć argumentu 'gammaDane'
#' gamma = 0.9
#' dane = pobierz_wartosci_wskaznikow_ewd("T", 2013:2014, tylkoWyswietlane = FALSE, gamma = gamma)
#' agr = agreguj_wskazniki_ewd(dane, zmiennaGrupujaca = "poczta", pu = TRUE, gammaDane = gamma)
#' }
#' @importFrom stats as.formula
#' @import dplyr
#' @import ZPD
#' @export
agreguj_wskazniki_ewd <- function(dane, poziom = NULL, grupujPoLatach = TRUE,
                                  zmiennaGrupujaca = "teryt_szkoly",
                                  tylkoWyswietlane = NA, pu = FALSE,
                                  gammaDane = 0.95, gamma = 0.95) {
  stopifnot(is.data.frame(dane),
            is.character(poziom) | is.null(poziom),
            is.logical(grupujPoLatach), length(grupujPoLatach) == 1,
            is.character(zmiennaGrupujaca), length(zmiennaGrupujaca) == 1,
            is.logical(tylkoWyswietlane), length(tylkoWyswietlane) == 1,
            is.logical(pu), length(pu) == 1,
            is.numeric(gamma), length(gamma) == 1)
  if (!is.null(poziom)) {
    stopifnot(poziom %in% c("gmina", "powiat", "województwo"),
              zmiennaGrupujaca == "teryt_szkoly")
  }
  stopifnot(grupujPoLatach %in% c(TRUE, FALSE),
            pu %in% c(TRUE, FALSE),
            gamma > 0, gamma < 1)
  opisoweNazwy = "TERYT gminy" %in% names(dane)
  if (opisoweNazwy) {
    names(dane) = konwertuj_nazwy_na_opisowe(names(dane), TRUE)
  }
  stopifnot("rok_do" %in% names(dane),
            "teryt_szkoly" %in% names(dane),
            "id_szkoly" %in% names(dane))
  if (!(zmiennaGrupujaca %in% names(dane))) {
    stop("Funkcja nie zawiera ",
         ifelse(zmiennaGrupujaca == "teryt_szkoly",
                "zmiennej opisującej TERYT.", "podanej zmiennej grupującej."))
  }
  if (!is.null(poziom)) {
    grupowanie = do.call(przygotuj_funkcje_grupujaca_teryt(poziom),
                         list(dane$teryt_szkoly))
    zmiennaGrupujaca = names(grupowanie)
    dane = bind_cols(dane, grupowanie)
  }
  if (grupujPoLatach) {
    rokDo = "rok_do"
  } else {
    rokDo = NULL
  }
  maskaPU = ifelse(pu, "|dg_pu_(srednia|ewd)", "")

  names(dane) = enc2native(names(dane))  # jako że select() nie radzi sobie z UTFem
  dane = dane[, c(zmiennaGrupujaca,
                  grep(paste0("^(id_szkoly|rok_do)$|^(ewd|lu_ewd|wyswietlaj|srednia",
                              maskaPU, ")[_ ]"),
                       names(dane), value = TRUE))]

  zmienne = c("id_szkoly", "rok_do", zmiennaGrupujaca)
  maskaZmienne = paste0("^(ewd|lu_ewd|wyswietlaj|srednia", maskaPU, ")[_ ]")
  dane = melt(dane[, c(zmienne,
                       grep(maskaZmienne, names(dane), value = TRUE))],
              id = zmienne) %>%
    mutate(
      wskaznik = gsub(maskaZmienne, "", .data$variable),
      variable = gsub(paste0(maskaZmienne, ".*$"), "\\1", .data$variable)
    )

  formulaTemp = as.formula(paste0(paste0(zmienne, collapse = "+"),
                                  " + wskaznik ~ variable"))
  dane = dcast(dane, formulaTemp, value.var = "value") %>%
    group_by({{rokDo}}, .data$wskaznik, {{zmiennaGrupujaca}})
  # Gdy tylkoWyswietlane to TRUE, odfiltruj niewyświetlane.
  if (tylkoWyswietlane %in% TRUE & "wyswietlaj" %in% names(dane)) {
    dane = dane %>% filter(.data$wyswietlaj == 1)
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

  if (pu) {
    lambda = sqrt(qchisq(gammaDane, 2))
    dane = dane %>%
      mutate(
        dg_pu_ewd = (.data$ewd - .data$dg_pu_ewd) / lambda,
        dg_pu_srednia = (.data$srednia - .data$dg_pu_srednia) / lambda
      ) %>%
      rename(
        bs_ewd = .data$dg_pu_ewd,
        bs_srednia = .data$dg_pu_srednia
      )
    lambda = sqrt(qchisq(gamma, 2))
  }
  dane = dane %>% 
    summarise(
      ewd_agr       = weighted.mean(.data$ewd, .data$lu_ewd, na.rm = TRUE),
      srednia_agr   = weighted.mean(.data$srednia, .data$lu_ewd, na.rm = TRUE),
      lu_sum        = sum(.data$lu_ewd, na.rm = TRUE),
      wyswietlaj    = any(.data$wyswietlaj == 1),
      bs_ewd        = sqrt(
        sum(.data$lu_ewd * .data$ewd^2, na.rm = TRUE) -
        sum(.data$lu_ewd * .data$ewd, na.rm = TRUE)^2 / .data$lu_sum +
        sum(.data$lu_ewd^2 * .data$bs_ewd^2, na.rm = TRUE)
      ) / .data$lu_sum,
      bs_srednia    = sqrt(
        sum(.data$lu_ewd * .data$srednia^2, na.rm = TRUE) -
        sum(.data$lu_ewd * .data$srednia, na.rm = TRUE)^2 / .data$lu_sum +
        sum(.data$lu_ewd^2 * .data$bs_srednia^2, na.rm = TRUE)
      ) / .data$lu_sum,
      dg_pu_ewd     = .data$ewd_agr - lambda * .data$bs_ewd,
      gg_pu_ewd     = .data$ewd_agr + lambda * .data$bs_ewd,
      dg_pu_srednia = .data$srednia_agr - lambda * .data$bs_srednia,
      gg_pu_srednia = .data$srednia_agr + lambda * .data$bs_srednia
    ) %>%
    rename(ewd = .data$ewd_agr, srednia = srednia_agr)
  if (!pu) {
    dane = dane %>%
      select(-.data$bs_ewd, -.data$bs_srednia, -.data$dg_pu_ewd, -.data$gg_pu_ewd, -.data$dg_pu_srednia, -.data$gg_pu_srednia)
  }
  if (pu) {
    dane = dane %>%
      select(-.data$bs_ewd, -.data$bs_srednia)
  }
  if (is.na(tylkoWyswietlane) & "wyswietlaj" %in% names(dane)) {
    dane$ewd[!dane$wyswietlaj] = NA
    dane$srednia[!dane$wyswietlaj] = NA
    if (pu) {
      dane$dg_pu_ewd[!dane$wyswietlaj] = NA
      dane$gg_pu_ewd[!dane$wyswietlaj] = NA
      dane$dg_pu_srednia[!dane$wyswietlaj] = NA
      dane$gg_pu_srednia[!dane$wyswietlaj] = NA
    }
  }
  dane = select(dane, -.data$wyswietlaj) %>%
    melt(c(rokDo, zmiennaGrupujaca, "wskaznik")) %>%
    dcast(as.formula(paste0(paste0(c(rokDo, zmiennaGrupujaca), collapse = "+"),
                            " ~ wskaznik + variable")))

  if (opisoweNazwy) {
    names(dane) = konwertuj_nazwy_na_opisowe(names(dane))
    names(dane) = gsub("_", " ", names(dane))
  }

  return(as.data.frame(dane))
}
#' @title Przygotowanie funkcji grupujacej w JST na podstawie TERYTu
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
                 return(tibble(teryt_gminy = teryt))
               },
               powiat = function(teryt){
                 return(tibble(teryt_powiatu = round(teryt / 100) * 100))
               },
               `województwo` = function(teryt){
                 return(tibble("teryt_wojewodztwa" = round(teryt / 10^4) * 10^4))
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
    c("teryt_gminy" , "TERYT gminy"),
    c("teryt_powiatu" , "TERYT powiatu"),
    c("teryt_wojewodztwa" , "TERYT województwa"),
    c("rodzaj_gminy" , "rodzaj gminy"),
    c("rok_do"       , "ostatni rok okresu obejmowanego przez wskaźnik"),
    c("^pomin$"      , "czy szkoła pomijana na stronie"),
    c("wyswietlaj"   , "czy elipsa wyświetlana -"),
    c("dg_pu_"       , "dolna granica przedz. ufności dla "),
    c("gg_pu_"       , "górna granica przedz. ufności dla "),
    c("srednia"      , "śr. wyników egzaminów"),
    c("_lu_sum$"     , " łączna liczba uczniów"),
    c("_lu_"         , " liczba uczniów "),
    c("lu_"          , "liczba uczniów "),
    c("_trend_EWD"   , " trend EWD"),
    c("ewd"          , "EWD"))
  klucz = data.frame(klucz)
  if (!revert) {
    colnames(klucz) = c("orginalne", "nowe")
  } else {
    colnames(klucz) = c("nowe", "orginalne")
    klucz$nowe = sub("[:^:]|[:$:]", "", klucz$nowe)
  }

  for (k in seq_len(nrow(klucz))) {
    nazwyKolumn = sub(klucz$orginalne[k], klucz$nowe[k], nazwyKolumn)
  }
  return(nazwyKolumn)
}
