#' @title Pobieranie wartosci wskaznikow EWD.
#' @description
#' Funkcja pobiera z bazy wartości wskaźników EWD. Domyślne wartości argumentów zostały
#' dobrane pod kątem wydawania wskazńików na zewnątrz.
#' @param typSzkoly ciąg znaków: 'gimn.', 'LO' lub 'T'
#' @param lata wektor liczb całkowitych - lata, dla których mają zostać pobrane wskaźniki
#' (wskaźniki identyfikowane są po ostatnim roku w ramach okresu, np. wskaźniki 2013-2011
#' uzyskamy podając jako argument \code{lata} wartość 2013)
#' @param zapis opcjonalnie nazwa pliku, do którego zostaną zapisane pobrane dane
#' (w formacie csv)
#' @param jst opcjonalnie wyrażenie regularne opisujące TERYTy JST, dla których mają
#' zostać zwrócone wskaźniki lub NULL, jeśli mają zostać zwrócone wartości wskaźników dla
#' całego kraju
#' @param idOke wartość logiczna (domyślnie FALSE) - czy dołączać kody OKE szkół?
#' @param daneAdresowe wartość logiczna (domyślnie TRUE) - czy dołączać dane adresowe
#' szkół?
#' @param opisoweNazwy wartość logiczna (domyślnie TRUE) - czy zmieniać nazwy kolumn na
#' pięknie opisowe?
#' @param lUcznPrzedm wartość logiczna (domyślnie FALSE) - czy mają zostać zwrócone
#' również liczby uczniów zdających poszczególne części egzaminu?
#' @param dodatkoweInfo wartość logiczna (domyślnie FALSE) - czy mają zostać zwrócone
#' pełne charakterystyki wskaźników, wraz z informacjami o korelacji średnich wyników
#' na wyjściu i EWD, kategorii i ew. o trendzie (jeśli jest dla danego wskaźnika) oraz
#' informacje o wielkości miejscowości, rodzaju gminy i rodzaju szkoły?
#' @param tylkoWskDoPrezentacji wartość logiczna (domyślnie TRUE) - czy mają zostać
#' zwrócone wartości tylko tych wskaźników, które są zatwierdzone do prezentacji na
#' stronie?
#' @param tylkoWyswietlane wartość logiczna (domyślnie TRUE) - czy mają zostać
#' zwrócone wartości tylko dla tych szkół, dla których wyswietlane są elipsy? (ze
#' względu na kategorię, jaka została im przypisana)
#' @param czyPomin wartość logiczna (domyślnie TRUE) - czy pominąć wartości wskaźników
#' dla szkół specjalnych itp. (tj. takich, które nie były uwzględniane w grupie szkół,
#' na których estymowane były modele EWD i całkowicie pomijanych przy prezentacji
#' wskaźników na stronie)?
#' @param gamma poziom ufności (liczba z przedziału [0;1] )
#' @param fileEncoding ciąg znaków - strona kodowa, w której zostanie zapisany wynikowy
#' plik csv
#' @return data frame
#' @import ZPD
#' @import reshape2
#' @import lazyeval
#' @export
pobierz_wartosci_wskaznikow_ewd = function(typSzkoly, lata, zapis = NULL, jst = NULL,
                                           idOke = FALSE, daneAdresowe = TRUE,
                                           opisoweNazwy = TRUE, lUcznPrzedm = FALSE,
                                           dodatkoweInfo = FALSE,
                                           tylkoWskDoPrezentacji = TRUE,
                                           tylkoWyswietlane = TRUE, czyPomin = TRUE,
                                           gamma = 0.95, fileEncoding = "windows-1250") {
  stopifnot(is.numeric(lata)        , length(lata) > 0,
            is.character(typSzkoly) , length(typSzkoly) == 1,
            is.null(jst) | is.character(jst), is.null(jst) | length(jst) == 1,
            is.null(zapis) | is.character(zapis),
            is.null(zapis) | length(zapis) == 1,
            is.logical(idOke)                , length(idOke) == 1,
            is.logical(daneAdresowe)         , length(daneAdresowe) == 1,
            is.logical(opisoweNazwy)           , length(opisoweNazwy) == 1,
            is.logical(lUcznPrzedm)          , length(lUcznPrzedm) == 1,
            is.logical(dodatkoweInfo)        , length(dodatkoweInfo) == 1,
            is.logical(tylkoWskDoPrezentacji), length(tylkoWskDoPrezentacji) == 1,
            is.logical(tylkoWyswietlane)     , length(tylkoWyswietlane) == 1,
            is.logical(czyPomin)             , length(czyPomin) == 1,
            is.numeric(gamma)                , length(gamma) == 1,
            is.character(fileEncoding)       , length(fileEncoding) == 1
  )
  stopifnot(idOke %in% c(TRUE, FALSE),
            daneAdresowe %in% c(TRUE, FALSE),
            opisoweNazwy %in% c(TRUE, FALSE),
            lUcznPrzedm %in% c(TRUE, FALSE),
            dodatkoweInfo %in% c(TRUE, FALSE),
            tylkoWskDoPrezentacji %in% c(TRUE, FALSE),
            tylkoWyswietlane %in% c(TRUE, FALSE),
            czyPomin %in% c(TRUE, FALSE),
            gamma < 1, gamma > 0, !is.na(gamma))
  # pobieranie z bazy
  if (length(lata) == 1) lata = rep(lata, 2)  # brzydkie, ale za to 4 wiersze dalej zadziała
  src = polacz()
  wskazniki = pobierz_wskazniki(src)
  wskazniki = filter_(wskazniki, ~ rodzaj_wsk == "ewd", ~ typ_szkoly == typSzkoly,
                      ~ rok_do %in% lata)
  wskazniki = select_(wskazniki, ~ -matches("^(opis_wsk|id_skali|skalowanie)$"))
  if (tylkoWskDoPrezentacji) wskazniki = filter_(wskazniki, ~ wsk_do_prezentacji == TRUE)
  wskazniki = select_(wskazniki, ~ -matches("^(rodzaj_egzaminu|czesc_egzaminu)$"))
  wskazniki = distinct(wskazniki)
  wskazniki = suppressMessages(left_join(wskazniki, pobierz_wartosci_wskaznikow(src)))
  if (tylkoWyswietlane) wskazniki = filter_(wskazniki, ~ wyswietlaj == TRUE)
  wskazniki = select_(wskazniki, ~ matches("^(wskaznik|skrot|rok_do|id_ww|id_szkoly)$"),
                      ~ matches("^(pomin|kategoria|wyswietlaj|srednia|bs|ewd|bs_ewd)$"),
                      ~ matches("^(trend_ewd|bs_trend_ewd|korelacja|lu_ewd|lu_wszyscy)$"))
  if (lUcznPrzedm) {
    message("Pobieranie informacji o liczbie zdających.")
    lUczniow = suppressMessages(left_join(wskazniki, pobierz_wartosci_wskaznikow_lu(src)))
    lUczniow = filter_(lUczniow, "!is.na(czesc_egzaminu)")
    lUczniow = select_(lUczniow, ~ matches("^(id_ww|czesc_egzaminu|przedm_lu)$"))
    lUczniow = as.data.frame(collect(lUczniow))
    for (i in names(lUczniow)[unlist(lapply(lUczniow, is.character))]) {
      Encoding(lUczniow[, i]) = "UTF-8"
      lUczniow[, i] = enc2native(lUczniow[, i])
    }
    lUczniow$czesc_egzaminu = paste0("l_uczn_", lUczniow$czesc_egzaminu)
    lUczniow = dcast(lUczniow, id_ww ~ czesc_egzaminu, identity, fill = NA_integer_,
                     value.var = "przedm_lu")
    names(lUczniow) = sub("podstawowa", "podst", names(lUczniow))
    names(lUczniow) = sub("rozszerzona", "rozsz", names(lUczniow))
    names(lUczniow) = gsub(" ", "_", names(lUczniow), fixed=TRUE)
  }
  message("Pobieranie informacji o wartościach wskaźników.")
  wskazniki = as.data.frame(collect(wskazniki))
  # dalsze przekształcanie
  message("Wyliczanie przedziałów ufności.")
  lambda = sqrt(qchisq(gamma, 2))
  dots = list(dg_pu_srednia = interp("srednia - lambda * bs", lambda = lambda),
              gg_pu_srednia = interp("srednia + lambda * bs", lambda = lambda),
              dg_pu_ewd     = interp("ewd - lambda * bs_ewd", lambda = lambda),
              gg_pu_ewd     = interp("ewd + lambda * bs_ewd", lambda = lambda))
  wskazniki = mutate_(wskazniki, .dots = dots)
  wskazniki = select_(wskazniki, ~ -matches("^bs(|_srednia|_ewd)$"))
  zmNaDlugi = c("kategoria", "wyswietlaj", "srednia", "ewd", "trend_ewd", "bs_trend_ewd",
                "korelacja", "lu_ewd", "dg_pu_srednia", "gg_pu_srednia", "dg_pu_ewd",
                "gg_pu_ewd")
  zmNaDlugi = colnames(wskazniki)[colnames(wskazniki) %in% zmNaDlugi]
  if (lUcznPrzedm) {
    zmNaDlugi = c(zmNaDlugi, names(lUczniow)[!(names(lUczniow) %in% names(wskazniki))])
    wskazniki = suppressMessages(left_join(wskazniki, lUczniow))
  }
  wskazniki = melt(wskazniki, id.vars = c("id_ww", "rok_do", "wskaznik", "skrot",
                                          "id_szkoly", "pomin", "lu_wszyscy"),
                   measure.vars = zmNaDlugi)
  # usuwanie niepotrzebnych zmiennych (które chwilowo są obserwacjami)
  if (!dodatkoweInfo) {
    wskazniki = filter_(wskazniki, ~ !(variable %in% c("kategoria", "wyswietlaj",
                                                       "korelacja")))
  }
  # przekształcanie do postaci szerokiej
  wskazniki = mutate_(wskazniki, .dots=list(variable="levels(variable)[variable]"))
  if (opisoweNazwy) {
    dots = list(variable = 'paste(variable, "wsk.", skrot)')
    nazwyWskaznikow = paste0(" wsk. ", unique(wskazniki$skrot))
  } else {
    dots = list(variable = 'paste(variable, wskaznik, sep="_")')
    nazwyWskaznikow = paste0("_", unique(wskazniki$wskaznik))
  }
  wskazniki = mutate_(wskazniki, .dots=dots)
  wskazniki = dcast(wskazniki, rok_do + id_szkoly + pomin + lu_wszyscy ~ variable,
                    identity, fill = NA_real_, value.var = "value")
  # łączenie z danymi szkół
  message("Pobieranie informacji o szkołach.")
  daneSzkol = pobierz_dane_szkol(c(lata, min(lata) - (1:2)), typSzkoly, idOke = idOke,
                                 daneAdresowe = daneAdresowe)
  if (!dodatkoweInfo) {
    daneSzkol = select_(daneSzkol, ~ -matches("^(publiczna|dla_doroslych|specjalna)$"),
                        ~ -matches("^(przyszpitalna|wielkosc_miejscowosci|rodzaj_gminy)$"))
  }
  if (!any(c("LO", "T") %in% typSzkoly)) {
    daneSzkol = select_(daneSzkol, ~ -matches("^(matura_miedzynarodowa)$"))
  }
  wskazniki = suppressMessages(inner_join(daneSzkol, wskazniki))
  # filtrowanie JST
  if(!is.null(jst)){
    wskazniki = filter_(wskazniki, ~ grepl(jst, teryt_szkoly))
  }
  # układanie kolumn w odpowiedniej kolejności
  nazwy = colnames(wskazniki)
  maska = paste(nazwyWskaznikow, collapse="|")
  maska = gsub("(", "[(]", maska, fixed=TRUE)
  maska = gsub(")", "[)]", maska, fixed=TRUE)
  maska = paste0("(", maska, ")$")
  sort1 = as.numeric(grepl(maska, nazwy)) + 2 * as.numeric(grepl("^l_uczn_", nazwy))
  sort2 = sub(maska, "", nazwy)
  sort2 = factor(sort2, levels=c("dg_pu_srednia", "gg_pu_srednia", "srednia",
                                 "dg_pu_ewd"    , "gg_pu_ewd"    , "ewd",
                                 "trend_ewd", "bs_trend_ewd", "korelacja",
                                 "lu_ewd", "kategoria", "wyswietlaj"))
  sort2 = as.numeric(sort2)
  sort2[is.na(sort2)] = 0
  sort3 = sub(paste0(".*", maska), "\\1", nazwy)
  sort3 = factor(sort3, levels = nazwyWskaznikow)
  sort3 = as.numeric(sort3)
  sort3[is.na(sort3)] = 0
  wskazniki = wskazniki[, order(sort1, sort2, sort3)]
  # ew. piękne nazwy kolumn
  if (czyPomin) {
    wskazniki = select_(wskazniki, ~ -matches("^(pomin)$"))
  }
  if (opisoweNazwy) {
    maska = grep("ewd[ _]|srednia[ _]|bs[ _]", names(wskazniki))
    wskazniki[maska] = lapply(wskazniki[maska], round, digits=2)
    names(wskazniki) = sub("id_szkoly" , "id szkoły w bazie EWD", names(wskazniki))
    names(wskazniki) = sub("typ_szkoly", "typ szkoły", names(wskazniki))
    names(wskazniki) = sub("dla_doroslych", "szkoła dla dorosłych", names(wskazniki))
    names(wskazniki) = sub("specjalna"    , "szkoła specjalna", names(wskazniki))
    names(wskazniki) = sub("przyszpitalna", "szkoła przyszpitalna", names(wskazniki))
    wskazniki$artystyczna[is.na(wskazniki$artystyczna)] = "ndt."
    names(wskazniki) = sub("artystyczna"  , "typ szkoły artystycznej", names(wskazniki))
    names(wskazniki) = sub("id_szkoly_oke", "kod egzaminacyjny szkoły", names(wskazniki))
    names(wskazniki) = sub("nazwa_szkoly" , "nazwa", names(wskazniki))
    names(wskazniki) = sub("miejscowosc"  , "miejscowość", names(wskazniki))
    names(wskazniki) = sub("pna"          , "kod pocztowy", names(wskazniki))
    names(wskazniki) = sub("wielkosc_miejscowosci", "wielkość miejscowości", names(wskazniki))
    names(wskazniki) = sub("teryt_szkoly" , "TERYT gminy", names(wskazniki))
    names(wskazniki) = sub("rodzaj_gminy" , "rodzaj gminy", names(wskazniki))
    names(wskazniki) = sub("rok_do"       , "ostatni rok okresu obejmowanego przez wskaźnik", names(wskazniki))
    names(wskazniki) = sub("^pomin$"      , "czy szkoła pomijana na stronie", names(wskazniki))
    names(wskazniki) = sub("wyswietlaj"   , "czy elipsa wyświetlana -", names(wskazniki))
    names(wskazniki) = sub("dg_pu_" , "dolna granica przedz. ufności dla ", names(wskazniki))
    names(wskazniki) = sub("gg_pu_" , "górna granica przedz. ufności dla ", names(wskazniki))
    names(wskazniki) = sub("srednia", "śr. wyników egzaminów", names(wskazniki))
    names(wskazniki) = sub("ewd"    , "EWD", names(wskazniki))
    names(wskazniki) = sub("^lu_"   , "liczba uczniów ", names(wskazniki))
    names(wskazniki) = sub("_trend_EWD", " trend EWD", names(wskazniki))
    names(wskazniki) = enc2native(names(wskazniki))
  }
  # porządki
  wskazniki = select_(wskazniki, ~ -matches("^(rok)$"))
  maska = unlist(lapply(wskazniki, function(x) {return(all(is.na(x)))}))
  wskazniki = wskazniki[, !maska]
 	# zapis
 	if (!is.null(zapis)) {
 	  message("Zapis.")
 	  write.csv2(wskazniki, zapis, row.names=FALSE, na="", fileEncoding=fileEncoding)
 	  invisible(wskazniki)
 	} else {
     return(wskazniki)
 	}
}
