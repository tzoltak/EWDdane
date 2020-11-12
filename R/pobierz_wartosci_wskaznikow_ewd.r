#' @title Pobieranie wartosci wskaznikow EWD
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
#' @param tylkoNiePomin wartość logiczna (domyślnie TRUE) - czy pominąć wartości wskaźników
#' dla szkół specjalnych itp. (tj. takich, które nie były uwzględniane w grupie szkół,
#' na których estymowane były modele EWD i całkowicie pomijanych przy prezentacji
#' wskaźników na stronie)?
#' @param gamma poziom ufności (liczba z przedziału [0;1] )
#' @param fileEncoding ciąg znaków - strona kodowa, w której zostanie zapisany wynikowy
#' plik csv
#' @param src połączenie z bazą danych (jeśli NULL, zostanie podjęta próba
#'   nawiązania połączenia za pomocą \code{ZPD::polacz()})
#' @details
#' Przykłady użycia - p. \href{http://zpd.ibe.edu.pl/doku.php?id=pobieranie_wartosci_ewd}{http://zpd.ibe.edu.pl/doku.php?id=pobieranie_wartosci_ewd}.
#' @return data frame
#' @importFrom stats qchisq
#' @importFrom utils write.csv2
#' @import ZPD
#' @import reshape2
#' @import lazyeval
#' @export
pobierz_wartosci_wskaznikow_ewd = function(typSzkoly, lata, zapis = NULL, jst = NULL,
                                           idOke = FALSE, daneAdresowe = TRUE,
                                           opisoweNazwy = TRUE, lUcznPrzedm = FALSE,
                                           dodatkoweInfo = FALSE,
                                           tylkoWskDoPrezentacji = TRUE,
                                           tylkoWyswietlane = TRUE,
                                           tylkoNiePomin = TRUE,
                                           gamma = 0.95, 
                                           fileEncoding = "windows-1250",
                                           src = NULL) {
  stopifnot(is.numeric(lata)        , length(lata) > 0,
            is.character(typSzkoly) , length(typSzkoly) == 1,
            is.null(jst) | is.character(jst), is.null(jst) | length(jst) == 1,
            is.null(zapis) | is.character(zapis),
            is.null(zapis) | length(zapis) == 1,
            is.logical(idOke)                , length(idOke) == 1,
            is.logical(daneAdresowe)         , length(daneAdresowe) == 1,
            is.logical(opisoweNazwy)         , length(opisoweNazwy) == 1,
            is.logical(lUcznPrzedm)          , length(lUcznPrzedm) == 1,
            is.logical(dodatkoweInfo)        , length(dodatkoweInfo) == 1,
            is.logical(tylkoWskDoPrezentacji), length(tylkoWskDoPrezentacji) == 1,
            is.logical(tylkoWyswietlane)     , length(tylkoWyswietlane) == 1,
            is.logical(tylkoNiePomin)        , length(tylkoNiePomin) == 1,
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
            tylkoNiePomin %in% c(TRUE, FALSE),
            gamma < 1, gamma > 0, !is.na(gamma))
  if (tylkoWskDoPrezentacji == FALSE) {
    tylkoWskDoPrezentacji = NA
  }
  # pobieranie z bazy
  if (length(lata) == 1) {
    lata = rep(lata, 2)  # brzydkie, ale za to 4 wiersze dalej zadziała
  }
  if (is.null(src)) {
    src = polacz()
  }
  wskazniki = src %>%
    pobierz_wskazniki(doPrezentacji = tylkoWskDoPrezentacji) %>%
    filter(.data$rodzaj_wsk == "ewd", .data$typ_szkoly == typSzkoly, .data$rok_do %in% lata) %>%
    select(-matches("^(opis_wsk|id_skali|skalowanie)$"),
            -matches("^(rodzaj_egzaminu|czesc_egzaminu)$")) %>%
    distinct()
  wskazniki =
    suppressMessages(left_join(wskazniki,
                               pobierz_wartosci_wskaznikow(src, czyPomin = !tylkoNiePomin)))
  if (tylkoWyswietlane) {
    wskazniki = filter(wskazniki, .data$wyswietlaj %in% TRUE)
  }
  wskazniki = select(wskazniki, matches("^(wskaznik|skrot|rok_do|id_ww|id_szkoly)$"),
                      matches("^(pomin|kategoria|wyswietlaj|srednia|bs|ewd|bs_ewd)$"),
                      matches("^(trend_ewd|bs_trend_ewd|korelacja|lu_ewd|lu_wszyscy)$"))
  if (lUcznPrzedm) {
    message("Pobieranie informacji o liczbie zdających.")
    lUczniow = suppressMessages(left_join(wskazniki, pobierz_wartosci_wskaznikow_lu(src))) %>%
      filter(!is.na(.data$czesc_egzaminu)) %>%
      select(matches("^(id_ww|czesc_egzaminu|przedm_lu)$")) %>%
      collect(n = Inf) %>%
      as.data.frame()
    lUczniow$czesc_egzaminu = paste0("l_uczn_", lUczniow$czesc_egzaminu)
    lUczniow = dcast(lUczniow, id_ww ~ czesc_egzaminu, identity, fill = NA_integer_,
                     value.var = "przedm_lu")
    names(lUczniow) = sub("podstawowa", "podst", names(lUczniow))
    names(lUczniow) = sub("rozszerzona", "rozsz", names(lUczniow))
    names(lUczniow) = gsub(" ", "_", names(lUczniow), fixed = TRUE)
  }
  message("Pobieranie informacji o wartościach wskaźników.")
  wskazniki = collect(wskazniki, n = Inf)
  if (nrow(wskazniki) == 0) {
    stop("Nie znaleziono żadnych wskaźników spełniających podane ktryteria.")
  }
  # dalsze przekształcanie
  message("Obliczanie przedziałów ufności.")
  lambda = sqrt(qchisq(gamma, 2))
  wskazniki = wskazniki %>%
    mutate(
      dg_pu_srednia = .data$srednia - lambda * .data$bs,
      gg_pu_srednia = .data$srednia + lambda * .data$bs,
      dg_pu_ewd     = .data$ewd - lambda * .data$bs_ewd,
      gg_pu_ewd     = .data$ewd + lambda * .data$bs_ewd
    ) %>%
    select(-matches("^bs(|_srednia|_ewd)$"))
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
    zmienneUsun = c("kategoria", "korelacja")
    if (tylkoWyswietlane) zmienneUsun = c(zmienneUsun, "wyswietlaj")
    wskazniki = filter(wskazniki, !(.data$variable %in% zmienneUsun))
  }
  # przekształcanie do postaci szerokiej
  wskazniki = mutate(wskazniki, variable = levels(.data$variable)[.data$variable])
  if (opisoweNazwy) {
    wskazniki = mutate(wskazniki,
                        variable = paste(.data$variable, "wsk.", .data$skrot)) %>%
      group_by(.data$rok_do, .data$id_szkoly, .data$variable) %>%
      mutate(n = n()) %>%
      ungroup() %>%
      mutate(variable = ifelse(.data$n == 1, .data$variable, paste0(.data$variable, " (", .data$wskaznik, ")")))
    nazwyWskaznikow = unique(sub("^ewd ", "",
                                 grep("^ewd ", wskazniki$variable, value = TRUE)))
  } else {
    wskazniki = mutate(wskazniki, variable = paste(.data$variable, .data$wskaznik, sep="_"))
    nazwyWskaznikow = paste0("_", unique(wskazniki$wskaznik))
  }
  wskazniki = dcast(wskazniki, rok_do + id_szkoly + pomin + lu_wszyscy ~ variable,
                    identity, fill = NA_real_, value.var = "value")
  # łączenie z danymi szkół
  message("Pobieranie informacji o szkołach.")
  daneSzkol = pobierz_dane_szkol(c(lata, min(lata) - (1:2)), typSzkoly, idOke = idOke, daneAdresowe = daneAdresowe, src = src)
  if (!dodatkoweInfo) {
    daneSzkol = select(daneSzkol, -matches("^(publiczna|dla_doroslych|specjalna)$"),
                        -matches("^(przyszpitalna|wielkosc_miejscowosci|rodzaj_gminy)$"))
  }
  if (!any(c("LO", "T") %in% typSzkoly)) {
    daneSzkol = select(daneSzkol, -matches("^(matura_miedzynarodowa)$"))
  }
  wskazniki = suppressMessages(inner_join(daneSzkol, wskazniki))
  # filtrowanie JST
  if (!is.null(jst)) {
    wskazniki = filter(wskazniki, grepl(jst, .data$teryt_szkoly))
  }
  # układanie kolumn w odpowiedniej kolejności
  nazwy = colnames(wskazniki)
  maska = paste(nazwyWskaznikow, collapse = "|")
  maska = gsub("(", "[(]", maska, fixed = TRUE)
  maska = gsub(")", "[)]", maska, fixed = TRUE)
  maska = paste0("(", maska, ")$")
  sort1 = as.numeric(grepl(maska, nazwy)) + 2 * as.numeric(grepl("^l_uczn_", nazwy))
  sort2 = sub(paste0(".*", maska), "\\1", nazwy)
  sort2 = factor(sort2, levels = nazwyWskaznikow)
  sort2 = as.numeric(sort2)
  sort2[is.na(sort2)] = 0
  sort3 = sub(paste0("[ _]", maska), "", nazwy)
  sort3 = factor(sort3, levels = c("dg_pu_srednia", "gg_pu_srednia", "srednia",
                                   "dg_pu_ewd"    , "gg_pu_ewd"    , "ewd",
                                   "trend_ewd", "bs_trend_ewd", "korelacja",
                                   "lu_ewd", "kategoria", "wyswietlaj"))
  sort3 = as.numeric(sort3)
  sort3[is.na(sort3)] = 0
  wskazniki = wskazniki[, order(sort1, sort2, sort3)]
  # ew. piękne nazwy kolumn
  if (tylkoNiePomin) {
    wskazniki = select(wskazniki, -matches("^(pomin)$"))
  }
  names(wskazniki) = enc2native(names(wskazniki))
  if (opisoweNazwy) {
    maska = grep("ewd[ _]|srednia[ _]|bs[ _]", names(wskazniki))
    wskazniki[maska] = lapply(wskazniki[maska], round, digits = 2)
    names(wskazniki) = sub("id_szkoly_oke", "kod egzaminacyjny szkoły", names(wskazniki))
    names(wskazniki) = sub("id_szkoly" , "id szkoły w bazie EWD", names(wskazniki))
    names(wskazniki) = sub("typ_szkoly", "typ szkoły", names(wskazniki))
    names(wskazniki) = sub("dla_doroslych", "szkoła dla dorosłych", names(wskazniki))
    names(wskazniki) = sub("specjalna"    , "szkoła specjalna", names(wskazniki))
    names(wskazniki) = sub("przyszpitalna", "szkoła przyszpitalna", names(wskazniki))
    wskazniki$artystyczna[is.na(wskazniki$artystyczna)] = "ndt."
    names(wskazniki) = sub("artystyczna"  , "typ szkoły artystycznej", names(wskazniki))
    names(wskazniki) = sub("nazwa_szkoly" , "nazwa", names(wskazniki))
    names(wskazniki) = sub("wielkosc_miejscowosci", "wielkość miejscowości", names(wskazniki))
    names(wskazniki) = sub("miejscowosc"  , "miejscowość", names(wskazniki))
    names(wskazniki) = sub("pna"          , "kod pocztowy", names(wskazniki))
    names(wskazniki) = sub("teryt_szkoly" , "TERYT gminy", names(wskazniki))
    names(wskazniki) = sub("rodzaj_gminy" , "rodzaj gminy", names(wskazniki))
    names(wskazniki) = sub("rok_do"       , "ostatni rok okresu obejmowanego przez wskaźnik", names(wskazniki))
    names(wskazniki) = sub("^pomin$"      , "czy szkoła pomijana na stronie", names(wskazniki))
    names(wskazniki) = sub("wyswietlaj"   , "czy elipsa wyświetlana -", names(wskazniki))
    names(wskazniki) = sub("dg_pu_" , "dolna granica przedz. ufności dla ", names(wskazniki))
    names(wskazniki) = sub("gg_pu_" , "górna granica przedz. ufności dla ", names(wskazniki))
    names(wskazniki) = sub("srednia", "śr. wyników egzaminów", names(wskazniki))
    names(wskazniki) = sub("ewd"    , "EWD", names(wskazniki))
    names(wskazniki) = sub("^lu_|^l_uczn_"   , "liczba uczniów ", names(wskazniki))
    names(wskazniki) = sub("bs_trend_EWD", "bs. trend EWD", names(wskazniki))
    names(wskazniki) = sub("trend_EWD", "trend EWD", names(wskazniki))
    names(wskazniki) = sub("matura_miedzynarodowa", "matura międzynarodowa", names(wskazniki))
  }
  # porządki
  wskazniki = select(wskazniki, -matches("^(rok)$"))
  maska = unlist(lapply(wskazniki, function(x) {return(all(is.na(x)))}))
  wskazniki = wskazniki[, !maska]
 	# zapis
 	if (!is.null(zapis)) {
 	  message("Zapis.")
 	  write.csv2(wskazniki, zapis, row.names = FALSE, na = "",
 	             fileEncoding = fileEncoding)
 	  invisible(wskazniki)
 	} else {
     return(wskazniki)
 	}
}
