#' @title Przygotowywanie danych do wyliczania modeli EWD
#' @description Funkcja przygotowuje pliki z danymi do wyliczania modeli EWD,
#' wykorzystujących wyniki surowe lub znormalizowane ekwikwantylowo
#' i zapisuje go w aktywnym katalogu.
#' @details Funkcja działa w oparciu o dane dostępne lokalnie, ściągnięte
#' wcześniej przy pomocy funkcji \code{\link{pobierz_wyniki_surowe}}. Normy
#' ekwikwantylowe pobierane są z bazy, z użyciem funkcji
#' \code{\link[ZPD]{normalizuj_ekwikwantylowo}}.
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
przygotuj_dane_surowe_do_ewd = function(katalogZDanymi, typSzkoly,
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
  skrotyNazwEgzaminow = list("sprawdzian" = "s", "egzamin gimnazjalny" = "g",
                             "matura" = "m")
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
    dane = formaty_zmiennych_baza_na_ewd(dane)
    class(dane) = c(class(dane), "daneDoWyliczaniaEwd", "daneSurowe",
                    "daneZnormalizowane")
    attributes(dane)$dataUtworzenia = Sys.time()
    attributes(dane)$egzaminNaWyjsciu = egzaminNaWyjsciu
    attributes(dane)$egzaminNaWejsciu = egzaminNaWejsciu
    attributes(dane)$lata = i:(i - liczbaRocznikow + 1)
    attributes(dane)$wydluzenie = wydluzenie
    attributes(dane)$normy = c(attributes(daneNaWyjsciu)$normy,
                               attributes(daneNaWejsciu)$normy)
    setwd(katalogRoboczy)
    save(dane, file = plikZapis)
    plikiZapis = c(plikiZapis, plikZapis)
    message("  Zapisano ", format(nrow(dane), big.mark = "'"), " obserwacji.")
  }
  invisible(plikiZapis)
}
#' @title Funkcje pomocnicze przy przygotowywaniu danych do wyliczania modeli EWD
#' @description Funkcja wczytuje podany plik z danymi kontekstowymi i wybiera
#' z niego informacje w sposób odpowiedni do łączenia go potem z wynikami
#' egzaminu traktowanymi jako wyniki "na wejściu" lub wyniki "na wyjściu".
#' @param nazwaPliku ciąg znaków - nazwa pliku .RData
#' @param czyWyjscie wartość logiczna - czy zwrócić dane w zakresie odpowiednim
#' do łączenia z wynikami egzaminu "na wyjściu" (jeśli \code{FALSE} - zwrócone
#' zostaną w formacie odpowiednim do łączenia z wyikami egzaminu "na wejściu")
#' @param lata opcjonalnie wektor liczb całkowitych - lata egzaminu, do jakich
#' ma zostać zawężony zwracany zestaw danych
#' @return data table
wczytaj_dane_kontekstowe = function(nazwaPliku, czyWyjscie = TRUE, lata = NULL) {
  stopifnot(is.character(nazwaPliku), length(nazwaPliku) == 1,
            all(czyWyjscie) %in% c(TRUE, FALSE), length(czyWyjscie) == 1,
            is.null(lata) | is.numeric(lata))

  obiekty = load(nazwaPliku)
  if (length(obiekty) != 1 | any(!grepl("^[sgm]Kontekstowe$", obiekty))) {
    stop("Nieprawidłowa struktura pliku z danymi kontekstowymi: '",
         nazwaPliku, "'.")
  }
  skrotEgzaminu = paste0("_", sub("Kontekstowe", "", obiekty))
  daneKontekstowe = get(obiekty)
  if (!is.null(lata)) {
    daneKontekstowe = subset(daneKontekstowe, get("rok") %in% lata)
  }
  if (czyWyjscie) {
    daneKontekstowe = subset(daneKontekstowe, get("populacja_wy") %in% TRUE)
    maska = paste0("^(plec|id|rok|laur|dysleksja|klasa|publiczna|specjalna)|",
                   "^(typ_szkoly|przyszpitalna|dla_doroslych|artystyczna|wiek)|",
                   "^pomin_szkole")
  } else {
    daneKontekstowe = subset(daneKontekstowe, get("populacja_we") %in% TRUE)
    maska = paste0("^(id|rok|laur|dysleksja)")
  }
  daneKontekstowe = daneKontekstowe[, grep(maska, names(daneKontekstowe))]

  maska = !grepl("^(id_obserwacji|plec|pomin_szkole)$|^laur_", names(daneKontekstowe))
  names(daneKontekstowe)[maska] = paste0(names(daneKontekstowe)[maska],
                                         skrotEgzaminu)
  return(daneKontekstowe)
}
#' @title Funkcje pomocnicze przy przygotowywaniu danych do wyliczania modeli EWD
#' @description Funkcja wczytuje wyniki surowe egzaminu z podanych plików,
#' łączy je w jeden zbiór i łączy z podanymi danymi kontekstowymi.
#' @param nazwyPlikow wektor ciągów znaków - nazwy plików z surowymi wynikami
#' egzaminu
#' @param daneKontekstowe opcjonalnie (acz typowo) data frame z danymi
#' kontekstowymi, typowo wynik wywołania funkcji
#' \code{\link{wczytaj_dane_kontekstowe}}
#' @import ZPD
#' @return data table
wczytaj_wyniki_egzaminu = function(nazwyPlikow, daneKontekstowe = NULL) {
  stopifnot(is.character(nazwyPlikow), length(nazwyPlikow) > 0,
            is.null(daneKontekstowe) | is.data.frame(daneKontekstowe))

  if (!is.null(daneKontekstowe)) {
    skrotEgzaminu = names(daneKontekstowe)[grepl("^rok_", names(daneKontekstowe))]
    skrotEgzaminu = sub("^rok", "", skrotEgzaminu)
  } else {
    skrotEgzaminu = ""
  }
  skaleTesty = list()
  normy = list()
  for (i in nazwyPlikow) {
    obiekty = load(i)
    # obsługa części egzaminu gimnazjalnego
    testy = pobierz_testy(polacz()) %>%
      filter_(~prefiks %in% c("gh", "gm"), ~is.na(arkusz), ~dane_ewd == TRUE) %>%
      collect()
    if (exists("gh_h") & exists("gh_p")) {
      gh = suppressMessages(inner_join(get("gh_h"),
                                       get("gh_p")[, names(get("gh_p")) != "id_testu"]))
      gh$id_testu = filter_(testy, ~prefiks == "gh", ~rok == gh$rok[1])$id_testu
      obiekty = c(obiekty, "gh")
    }
    if (exists("gm_m") & exists("gm_p")) {
      gm = suppressMessages(inner_join(get("gm_m"),
                                       get("gm_p")[, names(get("gm_p")) != "id_testu"]))
      gm$id_testu = filter_(testy, ~prefiks == "gm", ~rok == gm$rok[1])$id_testu
      obiekty = c(obiekty, "gm")
    }
    # idziemy po obiektach
    skale = pobierz_skale(polacz(), doPrezentacji = NA) %>%
      filter_(~opis_skalowania == "normalizacja ekwikwantylowa EWD") %>%
      collect()
    for (konstrukt in obiekty) {
      flaga = FALSE
      temp = get(konstrukt)
      if (all(c("wynikiSurowe", "czescEgzaminu") %in% class(temp))) {
        flaga = TRUE
        temp = zsumuj_punkty(temp)
        temp = filter_(temp, ~!is.na(wynik))
        # trochę szukania, żeby móc przypisać normy
        maskaSkali = paste0("^ewd;", konstrukt, "R;", temp$rok[1],"$")
        idSkali = filter_(skale, ~grepl(maskaSkali, opis_skali)) %>%
          arrange_(~skalowanie)
        if (nrow(idSkali) > 0) {
          skaleTesty = c(skaleTesty, list(unique(temp$id_testu)))
          names(skaleTesty)[length(skaleTesty)] = idSkali$id_skali[1]
        }
        grupa = last(filter_(idSkali, ~posiada_normy == TRUE)$grupa, default = NULL)
        skalowanie = last(filter_(idSkali, ~posiada_normy == TRUE)$skalowanie, default = NULL)
        idSkali = last(filter_(idSkali, ~posiada_normy == TRUE)$id_skali, default = NULL)
        if (!is.null(idSkali) & !is.null(skalowanie) & grupa == '') {
          temp = normalizuj_ekwikwantylowo(temp, src = polacz(), idSkali = idSkali,
                                           skalowanie = skalowanie, grupa = grupa)
          normy[[length(normy) + 1]] = pobierz_normy(polacz()) %>%
            filter_(~id_skali == idSkali, ~skalowanie == skalowanie, ~grupa == grupa) %>%
            as.data.frame() %>%
            mutate_(rok = ~temp$rok[1], konstrukt = ~konstrukt)
          names(normy)[length(normy)] = paste0(konstrukt, "_", temp$rok[1])
        } else if (is.null(idSkali)) {
          warning("Nie przypisano normalizacji ekwikwantylowej wynikom konstruktu '",
                  konstrukt, "' z ", temp$rok[1], " roku. ",
                  "W bazie nie ma zapisanych norm powiązanych ze skalą '",
                  substring(maskaSkali, 2, nchar(maskaSkali) - 1), "'.",
                  immediate. = TRUE)
        } else {
          warning("Nie przypisano normalizacji ekwikwantylowej wynikom konstruktu '",
                  konstrukt, "' z ", temp$rok[1], " roku. ", "Normy w bazie dla skali '",
                  substring(maskaSkali, 2, nchar(maskaSkali) - 1),
                  "' są zdefiniowane w podziale na kilka grup, co nie jest ",
                  "obsługiwane przez tę funkcję.", immediate. = TRUE)
        }
        # porządki w nazwach
        names(temp) = gsub("id_testu", paste0("id_testu_", konstrukt), names(temp))
        names(temp) = gsub("wynik_norm", paste0(konstrukt, "_norm"), names(temp))
        names(temp) = gsub("wynik", paste0(konstrukt, "_suma"), names(temp))
        if (exists("daneRok", inherits = FALSE)) {
          daneRok = suppressMessages(full_join(daneRok, temp))
        } else {
          daneRok = temp
        }
      }
    }
    if (!flaga) {
      stop(i)
    }
    if (exists("dane", inherits = FALSE)) {
      dane = bind_rows(dane, daneRok)
    } else {
      dane = daneRok
    }
    rm(daneRok)
  }
  maska = !grepl("^id_obserwacji$|^id_testu|_(suma|norm)$", names(dane))
  names(dane)[maska] = paste0(names(dane)[maska], skrotEgzaminu)
  if (!is.null(daneKontekstowe)) {
    dane = suppressMessages(inner_join(daneKontekstowe, dane))
  }
  attributes(dane)$skaleTesty = skaleTesty
  attributes(dane)$normy = normy
  return(dane)
}
