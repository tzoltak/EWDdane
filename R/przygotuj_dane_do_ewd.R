#' @title Przygotowywanie danych do obliczania modeli EWD
#' @description Funkcja przygotowuje pliki z danymi do obliczania modeli EWD,
#' wykorzystujących wyniki surowe lub znormalizowane ekwikwantylowo lub
#' wyskalowane - w zależności od tego, jakie znajdzie w katalogu podanym
#' argumentem \code{katalogZDanymi} - i zapisuje go w aktywnym katalogu.
#' @details
#' Funkcja działa w oparciu o dane dostępne lokalnie, ściągnięte
#' wcześniej przy pomocy funkcji \code{\link{pobierz_wyniki_surowe}} lub
#' \code{\link{pobierz_wyniki_wyskalowane}}.
#'
#' Normy ekwikwantylowe pobierane są z bazy, z użyciem funkcji
#' \code{\link[ZPD]{normalizuj}}.
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
#' @param src NULL połączenie z bazą danych IBE zwracane przez funkcję
#' \code{\link[ZPD]{polacz}}; pozwala posłużyć się niestandardowymi parametrami
#' połączenia
#' @return wektor z nazwami zapisanych plików (niewidocznie)
#' @param usunPusteKolumny opcjonalnie wartość logiczna - czy usunąć puste
#' kolumny opisujące grupę w skalowaniu, błędy standardowe oraz kolumnę 'nr_pv',
#' jeśli skalowanie nie generowało PV?
#' @import dplyr
#' @import ZPD
#' @export
przygotuj_dane_do_ewd = function(katalogZDanymi, typSzkoly,
                                 lataDo, liczbaRocznikow = 1,
                                 wydluzenie = 1, usunPusteKolumny = TRUE,
                                 src = NULL) {
  stopifnot(is.character(katalogZDanymi), length(katalogZDanymi) == 1,
            is.character(typSzkoly), length(typSzkoly) == 1,
            is.numeric(lataDo), length(lataDo) > 0,
            is.numeric(liczbaRocznikow), length(liczbaRocznikow) == 1,
            is.numeric(wydluzenie), length(wydluzenie) == 1,
            all(usunPusteKolumny %in% c(TRUE, FALSE)),
            length(usunPusteKolumny) == 1,
            is.src(src) | is.null(src)
  )
  stopifnot(typSzkoly %in% c("gimn.", "LO", "T"),
            dir.exists(katalogZDanymi))
  stopifnot(lataDo >= 2006, all(as.integer(lataDo) == lataDo),
            liczbaRocznikow %in% 0:3,
            wydluzenie %in% 0:2)
  if (is.null(src)) {
    src = ZPD::polacz()
  }

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
  plikiZDanymiKontekstowymi = c(paste0(egzaminNaWejsciu, "-kontekstowe.RData"),
                                paste0(egzaminNaWyjsciu, "-kontekstowe.RData"))
  if (any(!file.exists(plikiZDanymiKontekstowymi))) {
    stop("Nie znaleziono plików z danymi kontestowymi:\n  '",
         paste0(plikiZDanymiKontekstowymi[!file.exists(plikiZDanymiKontekstowymi)],
                collapse = "',\n  '"),
         "'.")
  }
  lataWejscie = max(lataDo - tok):(min(lataDo) - liczbaRocznikow + 1 - tok - wydluzenie)
  kontekstoweNaWejsciu =
    wczytaj_dane_kontekstowe(plikiZDanymiKontekstowymi[1], FALSE, lataWejscie)
  skrotEgzaminu = sub("e", "g", substr(egzaminNaWyjsciu, 1, 1))

  # tworzenie wynikowych plików
  for (i in lataDo) {
    # lataWyjście w pętli, bo od tego, co podamy wczytaj_dane_kontekstowe() zależy,
    # na jakiej grupie oblicza się wartość zmiennej 'lu_wszyscy'
    lataWyjscie = i:(i - liczbaRocznikow + 1)
    setwd(katalogZDanymi)
    kontekstoweNaWyjsciu =
      wczytaj_dane_kontekstowe(plikiZDanymiKontekstowymi[2], TRUE, lataWyjscie)
    if (liczbaRocznikow > 1) {
      rok = paste0(substring(as.character(i), 3, 4), "-",
                   substring(as.character(i - liczbaRocznikow + 1), 3, 4))
    } else {
      rok = substring(as.character(i), 3, 4)
    }
    plikZapis = paste0("dane_ewd_", sub("[.]", "", typSzkoly), rok, ".RData")
    message("Przygotowywanie pliku '", plikZapis, "'.")

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
    dane = formaty_zmiennych_baza_na_ewd(dane, usunPusteKolumny)
    dane = subset(dane, as.numeric(levels(get("wydl")))[get("wydl")] %in% (0:wydluzenie))
    dane$wydl = factor(as.numeric(levels(dane$wydl))[dane$wydl])

    # przypisywanie sum punktów skalom raschowym, które mają grupy
    skale = bind_rows(attributes(daneNaWyjsciu)$skale,
                      attributes(daneNaWejsciu)$skale)
    normy = c(attributes(daneNaWyjsciu)$normy,
              attributes(daneNaWejsciu)$normy)
    if (any(grepl("R_irt$", names(dane)))) {
      skaleNormy = filter(skale, grepl("^ewd;[^;]+R;", .data$opis_skali)) %>%
        select("id_skali", "skalowanie", "rok", "zmienna")
      normy = suppressMessages(
        inner_join(pobierz_normy(src), skaleNormy, copy = TRUE) %>%
          collect(n = Inf)
      )
      for (j in unique(normy$zmienna)) {
        skrotEgz = substr(j, 1, 1)
        temp = filter(normy, .data$zmienna == j) %>%
          select(-c("id_skali", "skalowanie", "zmienna")) %>%
          group_by(.data$grupa) %>%
          mutate(usun = .data$wartosc_zr == min(.data$wartosc_zr)) %>%
          mutate(usun = .data$usun & .data$wartosc != suppressWarnings(max(.data$wartosc[.data$usun]))) %>%
          filter(!.data$usun) %>%
          mutate(usun = .data$wartosc_zr == max(.data$wartosc_zr)) %>%
          mutate(usun = .data$usun & .data$wartosc != suppressWarnings(min(.data$wartosc[.data$usun]))) %>%
          filter(!.data$usun) %>%
          ungroup() %>%
          select(-"usun")
        names(temp) = sub("^wartosc$", paste0(sub("_irt$", "", j), "_suma"),
                          names(temp))
        names(temp) = sub("^wartosc_zr$", j, names(temp))
        names(temp) = sub("^rok$", paste0("rok_", skrotEgz), names(temp))
        if (all(temp$grupa == "" | is.na(temp$grupa))) {
          temp = select(temp, -"grupa")
        } else {
          names(temp) = sub("^grupa$", paste0("grupa_", j), names(temp))
        }
        dane = suppressMessages(left_join(dane, temp))
      }
      names(normy) = sub("zmienna", "konstrukt", names(normy))
      normy$konstrukt = sub("_irt$", "", normy$konstrukt)
      normyU = unique(normy[, c("konstrukt", "rok")])
      normyTemp = vector(mode = "list", length = nrow(normyU))
      names(normyTemp) = paste0(normyU$konstrukt, "_", normyU$rok)
      for (j in 1:nrow(normyU)) {
        normyTemp[[j]] = suppressMessages(semi_join(normy, normyU[j, ]))
      }
      normy = normyTemp
    }

    # tworzenie zmiennych opisujących przedmioty zdawane na maturze
    if (typSzkoly %in% c("LO", "T")) {
      message("  Dołączanie informacji o zdawanych przedmiotach.")
      zmianaKatalogu = try(setwd("../dane surowe"))
      if ("try-error" %in% class(zmianaKatalogu)) {
        warning("Nie udało się znaleźć katalogu z wynikami surowymi ",
                "('../dane surowe' w stosunku do katalogu podanego argumentem ",
                "'katalogZDanymi'), co uniemożliwia utworzenie zmiennych ",
                "opisujących przystępowanie przez zdających do poszczególnych ",
                "części egzaminu.", immediate. = TRUE)
      } else if (any(!file.exists(plikiNaWyjsciu))) {
        warning("W katalogu z wynikami surowymi ('../dane surowe' w stosunku do ",
                "katalogu podanego argumentem 'katalogZDanymi') nie znaleziono ",
                "pliku/ów:\n",
                paste0(paste0("'", plikiNaWyjsciu[!file.exists(plikiNaWyjsciu)],
                              "'"), collapse = ", "),
                "\nUniemożliwia to utworzenie zmiennych opisujących ",
                "przystępowanie przez zdających do poszczególnych części ",
                "egzaminu.", immediate. = TRUE)
      } else {
        przystepowanie = wczytaj_liczbe_przystepujacych(plikiNaWyjsciu)
        names(przystepowanie) = sub("^rok$", paste0("rok_", skrotEgzaminu),
                                    names(przystepowanie))
        dane = suppressMessages(left_join(dane, przystepowanie))
      }
      # dołączanie informacji o maturze międzynarodowej
      ib = pobierz_szkoly(src) %>%
        filter(.data$typ_szkoly == typSzkoly, .data$rok %in% lataWyjscie) %>%
        select("id_szkoly", "rok", "matura_miedzynarodowa") %>%
        collect(n = Inf)
      names(ib) = paste0(names(ib), "_", skrotEgzaminu)
      names(ib) = sub("^(matura_miedzynarodowa).*$", "\\1", names(ib))
      dane = suppressMessages(left_join(dane, ib))
    }

    class(dane) = c("daneDoWyliczaniaEwd", class(dane))
    if (any(grepl("_suma$", names(dane)))) {
      class(dane) = c("daneSurowe", class(dane))
    }
    if (any(grepl("_suma$", names(dane)))) {
      class(dane) = c("daneZnormalizowane", class(dane))
    }
    if (any(grepl("_irt$", names(dane)))) {
      class(dane) = c("daneWyskalowane", class(dane))
    }
    attributes(dane)$dataUtworzenia = Sys.time()
    attributes(dane)$egzaminNaWyjsciu = egzaminNaWyjsciu
    attributes(dane)$egzaminNaWejsciu = egzaminNaWejsciu
    attributes(dane)$lata = i:(i - liczbaRocznikow + 1)
    attributes(dane)$wydluzenie = wydluzenie
    attributes(dane)$normy = normy
    attributes(dane)$skale = skale
    setwd(katalogRoboczy)
    save(dane, file = plikZapis)
    plikiZapis = c(plikiZapis, plikZapis)
    message("  Zapisano ", format(nrow(dane), big.mark = "'"), " obserwacji.")
  }
  invisible(plikiZapis)
}
