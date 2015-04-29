#' @title Agregowanie wskaźników EWD
#' @description
#' Funkcja agreguje wskaźniki EWD.
#' @param dane ramka danych zwracana przez funkcję \link{pobierz_wartosci_wskaznikow_ewd}.
#' @param poziom ciąg znaków definiujący poziom agregacji: "gmina", "powiat" lub
#' "województwo" lub NULL,
#' jeśli podany został parametr 'funkcjaGrupujaca'
#' @param grupujPoLatach wartość logiczna (TRUE lub FALSE) wskazująca, czy przy
#' wyróżnianiu grup oprócz TERYTu ma być uwzględniona również zmienna opisująca
#' okres, dla którego wyliczony został wskaźnik
#' @param funkcjaGrupujaca fukcja przyjmująca jako pierwszy parametr teryt
#' szkoły i zwracająca jednoelementowy obiekt, która określa grupowanie
#' wskaźników ewd. W zwracanej ramce danych przez agreguj_wskazniki_ewd()
#' jedna kolumna będzie zawierać wynik funkcji grupującej
#' (jeżeli grupujPoLatach=FALSE) lub sześć ostatnich cyfr wyniku tej funkcji
#' (jeżeli grupujPoLatach=TRUE).
#' @param tylkoWyswietlane wartość logiczna (TRUE/FALSE/NA) opisująca, czy przy
#' wyliczaniu agregatów mają zostać uwzględnione tylko te szkoły, których elipsy
#' są pokazywane na stronie lub NA; przy TRUE uwzględniane są tylko te szkoły,
#' których elipsy są wyświetlane; przy FALSE uwzględniane są wszystkie szkoły,
#' bez względu na kategorię (ale z uwzględnieniem paramertu czyPomin); domyślna
#' wartość NA jest tożsama z FALSE z jednym wyjątkiem: jeśli w danej grupie nie
#' ma żadnej szkoły, której elipsa byłaby prezentowana, to wartość wskaźnika
#' danej grupy jest ustawiana na NA.
#' @param paramGrupujaca lista parametrów dodatkowych funkcji grupującej.
#' @param czyPomin parametr logiczny. Jeżeli TRUE to w obliczeniach pomijane są
#' szkoły specjalne, dla dorosłych i przyszpitalne.
#' @return data frame
#' @examples
#' \dontrun{
#' daneOrg = pobierz_wartosci_wskaznikow_ewd('T', c(2013, 2012),
#'  dodatkoweInfo = TRUE, jst = "^2605|^2601", opisoweNazwy = FALSE,
#'  tylkoWyswietlane = FALSE, tylkoWskDoPrezentacji = FALSE, czyPomin = FALSE)
#' jedenWiersz = daneOrg[1, ]
#' jedenWiersz$id_szkoly = 123
#' jedenWiersz$teryt_szkoly=123456
#' jedenWiersz$dla_doroslych= TRUE
#' jedenWiersz[, grepl("wyswietlaj_", names(jedenWiersz))][1] = 0
#' agreguj_wskazniki_ewd(jedenWiersz,
#'                       poziom = "powiat", grupujPoLatach = FALSE,
#'                        tylkoWyswietlane=NA, czyPomin = TRUE )
#' agreguj_wskazniki_ewd(jedenWiersz,
#'                       poziom = "powiat", grupujPoLatach = FALSE,
#'                        tylkoWyswietlane=FALSE, czyPomin = TRUE )
#' agreguj_wskazniki_ewd(jedenWiersz,
#'                       poziom = "powiat", grupujPoLatach = FALSE,
#'                        tylkoWyswietlane=TRUE, czyPomin = TRUE )
#'
#' agreguj_wskazniki_ewd(jedenWiersz,
#'                       poziom = "powiat", grupujPoLatach = FALSE,
#'                        tylkoWyswietlane=NA, czyPomin = FALSE )
#' agreguj_wskazniki_ewd(jedenWiersz,
#'                       poziom = "powiat", grupujPoLatach = FALSE,
#'                        tylkoWyswietlane=FALSE, czyPomin = FALSE )
#' agreguj_wskazniki_ewd(jedenWiersz,
#'                       poziom = "powiat", grupujPoLatach = FALSE,
#'                        tylkoWyswietlane=TRUE, czyPomin = FALSE )
#'
#' dane = rbind(daneOrg, jedenWiersz)
#' agreguj_wskazniki_ewd(dane,
#'                       poziom = "powiat", grupujPoLatach = TRUE,
#'                        tylkoWyswietlane=NA, czyPomin = TRUE )
#' agreguj_wskazniki_ewd(dane,
#'                       poziom = "powiat", grupujPoLatach = TRUE,
#'                        tylkoWyswietlane=FALSE, czyPomin = TRUE )
#' agreguj_wskazniki_ewd(dane,
#'                       poziom = "powiat", grupujPoLatach = TRUE,
#'                        tylkoWyswietlane=TRUE, czyPomin = TRUE )
#'
#' agreguj_wskazniki_ewd(dane,
#'                       poziom = "powiat", grupujPoLatach = TRUE,
#'                        tylkoWyswietlane=NA, czyPomin = FALSE )
#' agreguj_wskazniki_ewd(dane,
#'                       poziom = "powiat", grupujPoLatach = TRUE,
#'                        tylkoWyswietlane=FALSE, czyPomin = FALSE )
#' agreguj_wskazniki_ewd(dane,
#'                       poziom = "powiat", grupujPoLatach = TRUE,
#'                        tylkoWyswietlane=TRUE, czyPomin = FALSE
#' }
#' @import dplyr
#' @import lazyeval
#' @import ZPD
#' @export
agreguj_wskazniki_ewd <- function(dane, poziom = NULL, grupujPoLatach = TRUE,
                                  funkcjaGrupujaca = NULL, tylkoWyswietlane = NA,
                                  czyPomin = TRUE, paramGrupujaca = list()) {
  stopifnot(!is.null(poziom) | !is.null(funkcjaGrupujaca),
            poziom %in% c("gmina", "powiat", "województwo"))

  if (!is.null(funkcjaGrupujaca) & !is.null(poziom) ) {
    stop("Jeden z parametrów: 'poziom' lub 'funkcja_grupujaca' musi być NULLem.")
  } else if (!is.null(poziom)) {
    funkcjaGrupujaca = przygotuj_funkcje_grupujaca_teryt(poziom)
  }

  if ( "teryt_szkoly" %in% names(dane) ){
    if (grupujPoLatach) {
      rok_do =  "rok_do"
    } else {
      rok_do = NULL
    }

    grupowanie = do.call(funkcjaGrupujaca, append(list(dane$teryt_szkoly),
                                                  paramGrupujaca))
    if( !is.data.frame(grupowanie) |  ncol(grupowanie) != 1 |
        nrow(grupowanie) != nrow(dane) ){
      stop("Niepoprawny format danych zwracany przez funkcję grupującą.")
    }
    nazwaGrupowania = names(grupowanie)
    dane = cbind(dane %>% select_(~ -teryt_szkoly), grupowanie)

    #Jeżeli czyPomin true to usuń szkoły specjalne.
    if (czyPomin) {
      if ("pomin" %in% names(dane)) {
        dane = filter(dane, !pomin)
      } else {
        # milcząco zakładamy, że brak kolumny 'pomin' w danych to efekt wywołania
        # funkcji pobierz_wartosci_wskaznikow_ewd(..., czyPomin = TRUE)
      }
    }
    dane = dane[, grepl("^(id_szkoly|rok_do)$|^teryt|^(ewd|lu_ewd|wyswietlaj|srednia)_",
                        tolower(names(dane)))]

    zmienne = c("id_szkoly", "rok_do", nazwaGrupowania)
    maskaZmienne = "^(ewd|lu_ewd|wyswietlaj|srednia)_"
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
    if (!is.na(tylkoWyswietlane) & tylkoWyswietlane &
        "wyswietlaj" %in% names(dane))  {
      dane = dane %>% filter_(~ wyswietlaj == 1)
    }

    dane = dane %>%
      summarise(ewd =  weighted.mean(ewd, lu_ewd, na.rm = TRUE),
                srednia = weighted.mean(srednia, lu_ewd, na.rm = TRUE),
                lu_ewd = sum(lu_ewd, na.rm = TRUE),
                wyswietlaj = any(wyswietlaj == 1))
    if (is.na(tylkoWyswietlane) & "wyswietlaj" %in% names(dane)) {
      dane$ewd[!dane$wyswietlaj] = NA
      dane$srednia[!dane$wyswietlaj] = NA
    }
    dane = melt(dane, c(rok_do, nazwaGrupowania, "wskaznik")) %>%
      dcast(as.formula(paste0(paste0(c(rok_do, nazwaGrupowania), collapse = "+"),
                              " ~ wskaznik + variable")))
  } else if ("TERYT gminy" %in% names(dane) ){
    stop("Każ Grześkowi dokończyć funkcję.")
  } else{
    stop("Funkcja nie zawiera zmiennej opisującej teryt")
  }

  return(as.data.frame(dane))
}
#' @title Przygotowanie funkcji grupującej teryt.
#' @description
#' Funkcja przygotowuje funkcję grupującą. Więcej szczegółów w \link{agreguj_wskazniki_ewd}
#' @param poziom ciąg znaków definiujący poziom agregacji: "gmina", "powiat" lub "województwo" lub NULL,
#' jeśli podany został parametr 'funkcjaGrupujaca'
#' @return funkcja
przygotuj_funkcje_grupujaca_teryt <- function(poziom){
  stopifnot(poziom %in% c("gmina", "powiat", "województwo"))
  fun = switch(poziom,
               gmina = function(teryt){
                 return(data.frame(teryt_gminy=teryt))
               },
               powiat = function(teryt){
                 return(data.frame(teryt_powiatu=round(teryt/100)*100))
               },
               województwo = function(teryt){
                 return(data.frame(teryt_województwa=round(teryt/10000)*10000))
               }
  )
  return(fun)
}
