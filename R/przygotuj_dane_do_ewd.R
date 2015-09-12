#' @title Przygotowywanie danych do wyliczania modeli EWD
#' @description Funkcja przygotowuje pliki z danymi do wyliczania modeli EWD,
#' wykorzystujących wyniki surowe lub znormalizowane ekwikwantylowo lub
#' wyskalowane - w zależności od tego, jakie znajdzie w katalogu podanym
#' argumentem \code{katalogZDanymi} - i zapisuje go w aktywnym katalogu.
#' @details
#' Funkcja działa w oparciu o dane dostępne lokalnie, ściągnięte
#' wcześniej przy pomocy funkcji \code{\link{pobierz_wyniki_surowe}} lub
#' \code{\link{pobierz_wyniki_wyskalowane}}.
#'
#' Normy #' ekwikwantylowe pobierane są z bazy, z użyciem funkcji
#' \code{\link[ZPD]{normalizuj_ekwikwantylowo}}.
#'
#' Reguły wyboru, które wyniki zostaną zwrócone w sytuacji, gdy wykorzystywane
#' są wyniki wyskalowane i dla jakiejś skali dostępne są wyniki z kilku różńych
#' skalowań, opisane są w pomocy do funkcji
#' \code{\link{wczytaj_wyniki_wyskalowane}}.
#' @param katalogZDanymi ciąg znaków - ścieżka do katalogu, w którym znajdują
#' się pliki .RData z wynikami surowymi egzaminów i danymi kontekstowymi
#' o uczniach i szkołach
#' @param typSzkoly ciąg znaków: "gimn." | "LO" | "T"
#' @param lataDo wektor liczb całkowitych - lata egzaminu "na wyjściu", dla
#' których mają zostać przygotowane zbiory; w przypadku plików obejmujących'
#' kilka roczników absolwentów (tj. gdy \code{liczbaRocznikow > 1}) wskazuje
#' najpóźniejsze lata w ramach plików
#' @param liczbaRocznikow liczba całkowita - ile roczników absolwentów ma
#' obejmować pojedynczy plik (domyślnie 1 - zbiory "jednoroczne")
#' @param wydluzenie liczba całkowita - w przygotowanym zbiorze znajdą się
#' uczniowie o toku kształcenia wydłużonym maksymalnie o tyle lat
#' @return wektor z nazwami zapisanych plików (niewidocznie)
#' @export
przygotuj_dane_do_ewd = function(katalogZDanymi, typSzkoly,
                                        lataDo, liczbaRocznikow = 1,
                                        wydluzenie = 1) {
  stopifnot(is.character(katalogZDanymi), length(katalogZDanymi) == 1,
            is.character(typSzkoly), length(typSzkoly) == 1,
            is.numeric(lataDo), length(lataDo) > 0,
            is.numeric(liczbaRocznikow), length(liczbaRocznikow) == 1,
            is.numeric(wydluzenie), length(wydluzenie) == 1)
  stopifnot(typSzkoly %in% c("gimn.", "LO", "T"),
            dir.exists(katalogZDanymi))
  stopifnot(lataDo >= 2006, all(as.integer(lataDo) == lataDo),
            liczbaRocznikow %in% 0:3,
            wydluzenie %in% 0:2)

  egzaminNaWejsciu = list("gimn." = "sprawdzian",
                          "LO" = "egzamin gimnazjalny",
                          "T"  = "egzamin gimnazjalny")[typSzkoly][[1]]
  egzaminNaWyjsciu = list("gimn." = "egzamin gimnazjalny",
                          "LO" = "matura",
                          "T"  = "matura")[typSzkoly][[1]]

  tok = list("gimn." = 3, "LO" = 3, "T"  = 4)[typSzkoly][[1]]
  plikiZapis = vector(mode = "character")
  katalogRoboczy = getwd()
  on.exit(setwd(katalogRoboczy))

  # wczytywanie danych kontekstowych
  message("Wczytywanie danych kontekstowych.")
  setwd(katalogZDanymi)
  katalogZDanymi = getwd()
  plikiZDanymi = c(paste0(egzaminNaWejsciu, "-kontekstowe.RData"),
                   paste0(egzaminNaWyjsciu, "-kontekstowe.RData"))
  if (any(!file.exists(plikiZDanymi))) {
    stop("Nie znaleziono plików z danymi kontestowymi:\n  '",
            paste0(plikiZDanymi[!file.exists(plikiZDanymi)], collapse = "',\n  '"),
            "'.")
  }
  lataWejscie = max(lataDo - tok):(min(lataDo) - liczbaRocznikow + 1 - tok - wydluzenie)
  lataWyjscie = max(lataDo):(min(lataDo) - liczbaRocznikow + 1)
  kontekstoweNaWejsciu = wczytaj_dane_kontekstowe(plikiZDanymi[1], FALSE, lataWejscie)
  kontekstoweNaWyjsciu = wczytaj_dane_kontekstowe(plikiZDanymi[2], TRUE, lataWyjscie)
  skrotEgzaminu = sub("e", "g", substr(egzaminNaWyjsciu, 1, 1))

  # tworzenie wynikowych plików
  for (i in lataDo) {
    if (liczbaRocznikow > 1) {
      rok = paste0(substring(as.character(i), 3, 4), "-",
                   substring(as.character(i - liczbaRocznikow + 1), 3, 4))
    } else {
      rok = substring(as.character(i), 3, 4)
    }
    plikZapis = paste0("dane_ewd_", sub("[.]", "", typSzkoly), rok, ".RData")
    message("Przygotowywanie pliku '", plikZapis, "'.")

    setwd(katalogZDanymi)
    plikiNaWejsciu = paste0(egzaminNaWejsciu, " ",
                           (i - tok):(i - tok - liczbaRocznikow + 1 - wydluzenie),
                           ".RData")
    plikiNaWyjsciu = paste0(egzaminNaWyjsciu, " ",
                           i:(i - liczbaRocznikow + 1), ".RData")
    plikiZDanymi = c(plikiNaWejsciu, plikiNaWyjsciu)
    if (any(!file.exists(plikiZDanymi))) {
      warning("Nie udało się utworzyć pliku obejmującego roczniki absolwentów z lat ",
              i, "-", i - liczbaRocznikow + 1, ". Nie znaleziono plików z wynikami:\n  '",
              paste0(plikiZDanymi[!file.exists(plikiZDanymi)], collapse = "',\n  '"),
              "'.", immediate. = TRUE)
      next
    }
    daneNaWejsciu = try(wczytaj_wyniki_egzaminu(plikiNaWejsciu, kontekstoweNaWejsciu),
                        silent = TRUE)
    if ("try-error" %in% class(daneNaWejsciu)) {
      warning("Nie udało się utworzyć pliku obejmującego roczniki absolwentów z lat ",
              i, "-", i - liczbaRocznikow + 1, ". W pliku z wynikami: '",
              attributes(daneNaWejsciu)$condition$message,
              "' nie znaleziono żadnego obiektu z surowymi wynikami egzaminów.",
              immediate. = TRUE)
      next
    }
    daneNaWyjsciu = try(wczytaj_wyniki_egzaminu(plikiNaWyjsciu, kontekstoweNaWyjsciu),
                        silent = TRUE)
    if ("try-error" %in% class(daneNaWyjsciu)) {
      warning("Nie udało się utworzyć pliku obejmującego roczniki absolwentów z lat ",
              i, "-", i - liczbaRocznikow + 1, ". W pliku z wynikami: '",
              attributes(daneNaWyjsciu)$condition$message,
              "' nie znaleziono żadnego obiektu z surowymi wynikami egzaminów.",
              immediate. = TRUE)
      next
    }

    dane = suppressMessages(left_join(daneNaWyjsciu, daneNaWejsciu))
    dane = subset(dane, get(paste0("typ_szkoly_", skrotEgzaminu)) == typSzkoly)
    dane = formaty_zmiennych_baza_na_ewd(dane)
    class(dane) = c(class(dane), "daneDoWyliczaniaEwd")
    if (any(grepl("_suma$", names(dane)))) {
      class(dane) = c(class(dane), "daneSurowe")
    }
    if (any(grepl("_suma$", names(dane)))) {
      class(dane) = c(class(dane), "daneZnormalizowane")
    }
    if (any(grepl("_irt$", names(dane)))) {
      class(dane) = c(class(dane), "daneWyskalowane")
    }
    attributes(dane)$dataUtworzenia = Sys.time()
    attributes(dane)$egzaminNaWyjsciu = egzaminNaWyjsciu
    attributes(dane)$egzaminNaWejsciu = egzaminNaWejsciu
    attributes(dane)$lata = i:(i - liczbaRocznikow + 1)
    attributes(dane)$wydluzenie = wydluzenie
    attributes(dane)$normy = c(attributes(daneNaWyjsciu)$normy,
                               attributes(daneNaWejsciu)$normy)
    attributes(dane)$skale = rbind(attributes(daneNaWyjsciu)$skale,
                                   attributes(daneNaWejsciu)$skale)
    setwd(katalogRoboczy)
    save(dane, file = plikZapis)
    plikiZapis = c(plikiZapis, plikZapis)
    message("  Zapisano ", format(nrow(dane), big.mark = "'"), " obserwacji.")
  }
  invisible(plikiZapis)
}