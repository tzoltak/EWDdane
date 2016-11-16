#' @title Pobieranie surowych wynikow egzaminu
#' @description
#' Funkcja pobiera wyskalowane wyniki egzaminu (ze wszystkich lat i wszystkich
#' skal, powiązanych z danym egzaminem, które są w bazie) i zapisuje je na dysku
#' w postaci plików RData.
#' \itemize{
#'   \item{Funkcja sprawdzi, czy w aktywnym katalogu istnieje katalog
#'         \code{dane wyskalowane}. Jeśli tak, zapisze tam pliki, jeśli nie,
#'         najpierw go utworzy, a potem zapisze tam pliki.}
#'   \item{Dane dotyczące wyników z poszczególnych lat zapisane zostaną
#'         w oddzielnych plikach, o nazwach postaci \code{nazwa egzaminu rok.RData}.}
#'   \item{W każdym takim pliku znajduje się data frame klasy
#'         \code{wynikiWyskalowane}, o nazwie postaci \code{prefiksWyskalowane}
#'         (gdzie \code{prefiks} opisuje rodzaj egzaminu: \code{s} - sprawdzian,
#'         \code{g} - egz. gimn., \code{m} - matura). Przechowuje on wyskalowane
#'         wyniki w postaci długiej, analogicznie jak wynik wywołania funkcji
#'         \code{\link[ZPD]{pobierz_oszacowania_uczniow}}.}
#'   \item{Wśród atrybutów takiego obiektu znajduje się element \code{skale},
#'         który zawiera trochę dodatkowych informacji o skalach.}
#'   \item{Jeśli dla danego egzaminu nie znaleziono żadnych wyników
#'         wyskalowanych, to obiekt ten będzie nie data framem, ale NULLem!}
#' }
#' Dodatkowo, o ile parametr \code{daneKontekstowe} ma wartość \code{TRUE},
#' w tym samym katalogu zapisany zostanie plik \code{nazwa egzaminu - kontekstowe.RData},
#' zawierający ramkę danych o nazwie \code{prefiksKontekstowe} (gdzie
#' \code{prefiks} opisuje rodzaj egzaminu: \code{s} - sprawdzian, \code{g} - egz.
#' gimn., \code{m} - matura), będącą wynikiem wywołania funkcji
#' \code{\link{pobierz_dane_kontekstowe}}.
#' @param rodzajEgzaminu ciąg znaków - rodzaj egzaminu
#' @param lata NULL lub wektor liczb - lata, dla których mają zostać pobrane
#' wyniki; jeśli NULL pobrane zostaną wyniki z wszystkich dostępnych lat
#' @param nadpisz wartość logiczna - jeśli w miejscu, w którym mają być zapisane
#' wyniki są już pliki z wynikami, to czy je nadpisać? jeśli FALSE, pobrane
#' zostaną wyniki tylko z lat, dla których pliki jeszcze nie istnieją
#' @param daneKontekstowe wartość logiczna - czy pobrać również plik z danymi
#' kontekstowymi (tj. o uczniach i szkołach)?
#' @param src opcjonalnie połączenie z bazą danych IBE zwracane przez funkcję
#' \code{\link[ZPD]{polacz}}; pozwala posłużyć się połączeniem o wyższych niż
#' domyślne prawach dostępu, co ma znaczenie dla zakresu pobieranych danych
#' kontekstowych
#' @param katalogWyskalowane opcjonalnie ciąg znaków - ścieżka do katalogu,
#' w którym znajdują się pliki .RData z wynikami skalowania, zapisane przez
#' funkcje \code{\link[EWDskale]{skaluj_spr}},
#' \code{\link[EWDskale]{skaluj_egz_gimn}} lub
#' \code{\link[EWDskale]{skaluj_matura}}. Jeśli podany, wyniki wyskalowane
#' zostaną wczytane z tych plików, a nie z bazy.
#' @return lista z nazwami zapisanych plików (niewidocznie)
#' @importFrom stats setNames
#' @import ZPD
#' @export
pobierz_wyniki_wyskalowane = function(rodzajEgzaminu, lata = NULL,
                                      nadpisz = FALSE, daneKontekstowe = TRUE,
                                      src = NULL, katalogWyskalowane = NULL) {
  stopifnot(
    is.character(rodzajEgzaminu), length(rodzajEgzaminu) == 1,
    all(rodzajEgzaminu %in% c("sprawdzian", "egzamin gimnazjalny", "matura")),
    is.numeric(lata) | is.null(lata), length(lata) > 0 | is.null(lata),
    all(nadpisz %in% c(TRUE, FALSE)), length(nadpisz) == 1,
    all(daneKontekstowe %in% c(TRUE, FALSE)), length(daneKontekstowe) == 1,
    is.src(src) | is.null(src),
    is.character(katalogWyskalowane) | is.null(katalogWyskalowane)
  )
  if (!is.null(katalogWyskalowane)) {
    stopifnot(length(katalogWyskalowane) == 1)
    stopifnot(dir.exists(katalogWyskalowane))
  }
  czyZamykacSrc = FALSE
  if (is.null(src)) {
    src = polacz()
    czyZamykacSrc = TRUE
  }
  if (is.null(lata)) {
    lata = pobierz_testy(src) %>%
      filter_(~rodzaj_egzaminu == rodzajEgzaminu) %>%
      select_(.dots = ~rok) %>%
      distinct() %>%
      collect(n = Inf) %>%
      as.list() %>%
      unlist() %>%
      sort() %>%
      unname()
  }
  skrotEgzaminu = sub("e", "g", substr(rodzajEgzaminu, 1, 1))

  # sprawdzanie, co jest na dysku
  if (!dir.exists("dane wyskalowane")) {
    dir.create("dane wyskalowane")
    message("Utworzono katalog 'dane wyskalowane' w aktywnym katalogu:\n'", getwd(),"'\n")
  }
  czyPobrane = file.exists(paste0("dane wyskalowane/", rodzajEgzaminu, " ",
                                  lata, ".RData"))
  if (any(czyPobrane)) {
    message("Istnieją już zapisane pliki z wynikami tego egzaminu z lat: ",
            paste0(lata[czyPobrane], collapse = ", "), ".\n",
            ifelse(nadpisz,
                   "Zostanie on nadpisany nowo pobranymi danymi.\n",
                   "Podjęta zostanie próba dopisania do niego nowych danych.\n"))
  } else {
    stareOszacowania = NULL
  }
  if (daneKontekstowe) {
    if (file.exists(paste0("dane wyskalowane/", rodzajEgzaminu,
                           "- kontekstowe.RData"))) {
      message("Istnieje już zapisany plik z danymi kontekstowymi. ",
              "Zostanie on nadpisany nowymi danymi.\n")
    }
  }

  # pobieranie informacji o skalach
  # trochę zachodu z odsianiem powiązań testy-skale idących nie przez 'skale_testy'
  # a przez wspólne kryteria oceny
  skale = suppressMessages(
    pobierz_skale(src, doPrezentacji = NA, czyKtt = FALSE) %>%
      filter_(~rodzaj_egzaminu == rodzajEgzaminu, ~rodzaj_skali == "ewd",
              ~rok %in% c(lata, lata)) %>%
      left_join(pobierz_testy(src) %>% select_(~id_testu, ~czy_egzamin)) %>%
      select_(~-id_testu, ~-grupa, ~-posiada_normy) %>%
      collect(n = Inf) %>%
      distinct() %>%
      group_by_(~id_skali, ~skalowanie) %>%
      mutate_(.dots = setNames(list(~!czy_egzamin | all(czy_egzamin)),
                               "czy_egzamin")) %>%
      ungroup() %>%
      filter_(~czy_egzamin) %>%
      select_(~-czy_egzamin)
  )
  names(skale) = sub("^estymacja$", "skala_estymacja", names(skale))

  # pobieranie wyników
  message(rodzajEgzaminu, "\n", format(Sys.time(), "(%Y.%m.%d, %H:%M:%S)\n"))
  # pobieranie i zapis wyników
  for (i in 1:length(lata)) {
    message("Rok ", lata[i], ":")
    skaleRok = filter_(skale, ~rok == lata[i])

    # ew. wczytywanie już zapisanych oszacowan
    if (czyPobrane[i]) {
      if (!nadpisz) {
        stareOszacowania = load(paste0("dane wyskalowane/", rodzajEgzaminu, " ",
                                       lata[i], ".RData"))
        stareOszacowania = mget(paste0(skrotEgzaminu, "Wyskalowane"),
                                ifnotfound = list(NULL))[[1]]  # w przyszłości możnaprzejść na get0()
      } else {
        stareOszacowania = NULL
      }
    } else {
      stareOszacowania = NULL
    }

    if (is.null(katalogWyskalowane)) {
      oszacowania = suppressMessages(
        pobierz_oszacowania_uczniow(src) %>%
          semi_join(select_(skaleRok, ~id_skali, ~rok), copy = TRUE) %>%
          collect(n = Inf)
      )
      skalowaniaZDysku = NULL
    } else {
      pliki = list.files(katalogWyskalowane, paste0(lata[i], "Skalowanie.RData$"),
                         full.names = TRUE)
      oszacowania = wczytaj_wyniki_skalowania(pliki)
      skalowaniaZDysku = attributes(oszacowania)$skale
      if (!("rok" %in% names(oszacowania))) {
        oszacowania = within(oszacowania, {rok = lata[[i]]})
      }
    }

    # kosmetyka i zapis
    if (ncol(oszacowania) == 0) {
      oszacowania = NULL
    } else {
      # sprawdzanie, czy nie ma konfliktów z tym, co już na dysku i ew. dopisanie
      oszacowania = select_(oszacowania, ~id_skali, ~skalowanie, ~id_obserwacji,
                            ~rok, ~nr_pv, ~wynik, ~bs, ~grupa)
      if (!is.null(stareOszacowania) & !nadpisz) {
        lNowych = nrow(oszacowania)
        oszacowania = bind_rows(stareOszacowania, oszacowania) %>% distinct()
        lRoznych = select_(oszacowania, ~id_skali, ~skalowanie, ~id_obserwacji) %>%
          distinct() %>%
          nrow()
        message(" Wśród ", format(nrow(stareOszacowania), big.mark = "'"),
                "  wcześniej zapisanych\n     i ", format(lNowych, big.mark = "'"),
                "  właśnie wczytanych oszacowań są:\n * ",
                format(lNowych + nrow(stareOszacowania) - lRoznych,
                       big.mark = "'"), " rekordy/ów wspólne/ych;\n",
                " * ", format(nrow(oszacowania) - lRoznych, big.mark = "'"),
                " konflikty/ów.")
        if (nrow(oszacowania) > lRoznych) {
          stop("Wykryto konflikty pomiędzy wcześniej zapisanymi danymi, ",
               "a danymi właśnie pobieranymi. Usuń konflikty i spróbuj ponownie, ",
               "lub nadpisz wcześniej zapisane dane, wywołując funkcję ",
               "z argumentem nadpisz = TRUE.")
        }
        message( " Do danych dopisane zostanie ",
                 format(nrow(oszacowania) - nrow(stareOszacowania)),
                 " nowy/e/ych rekord(y/ów).\n",
                 " Informacje o skalach zostaną nadpisane nowymi, ",
                 "właśnie pobranymi z bazy.")
      }
      # ew. przyłącznie informacji o skalowaniach wczytanych z dysku
      if (!is.null(skalowaniaZDysku)) {
        names(skalowaniaZDysku) = sub("^opis$", "opis_skalowania",
                                      names(skalowaniaZDysku))
        names(skalowaniaZDysku) = sub("^data$", "data_skalowania",
                                      names(skalowaniaZDysku))
        names(skalowaniaZDysku) = sub("^do_prezentacji$", "skalowanie_do_prezentacji",
                                      names(skalowaniaZDysku))
        names(skalowaniaZDysku) = sub("^estymacja$", "skala_estymacja",
                                      names(skalowaniaZDysku))
        # dopisujemy informacje o skalach do wczytanych skalowań
        skaleTemp = skaleRok[, !(names(skaleRok) %in%
                                   intersect(names(skaleRok),
                                             names(skalowaniaZDysku))) |
                               names(skaleRok) %in% "id_skali"] %>% distinct()
        skalowaniaZDysku = suppressMessages(left_join(skalowaniaZDysku, skaleTemp))
        # i nadpisujemy/dołączamy do informacje/i o skalowaniach z bazy
        skaleRok = suppressMessages(
          anti_join(skaleRok, select_(skalowaniaZDysku, ~id_skali, ~skalowanie)) %>%
            bind_rows(skalowaniaZDysku)
        )
      }
      skaleRok = suppressMessages(semi_join(skaleRok, oszacowania))

      # końcowa estetyka
      attributes(oszacowania)$skale = skaleRok
      attributes(oszacowania)$dataPobrania = Sys.time()
      class(oszacowania) = append(class(oszacowania), c("wynikiWyskalowane"))
    }
    assign(paste0(skrotEgzaminu, "Wyskalowane"), oszacowania)
    rm(oszacowania)

    nazwaPliku = paste0("dane wyskalowane/", rodzajEgzaminu, " ", lata[i], ".RData")
    save(list = paste0(skrotEgzaminu, "Wyskalowane"), file = nazwaPliku)
    message(" Zapisano do pliku: ", nazwaPliku,
            format(Sys.time(), "\n (%Y.%m.%d, %H:%M:%S)\n"))
  }
  pliki = paste0("dane wyskalowane/", rodzajEgzaminu, " ", lata, ".RData")

  # pobieranie i zapis danych kontekstowych
  if (daneKontekstowe & !is.null(get(paste0(skrotEgzaminu, "Wyskalowane")))) {
    message("\nDane o uczniach i szkołach:")
    temp = pobierz_dane_kontekstowe(src, rodzajEgzaminu)
    class(temp) = append(class(temp), "daneKontekstowe")
    attributes(temp)$dataPobrania = Sys.time()
    assign(paste0(skrotEgzaminu, "Kontekstowe"), temp)
    rm(temp)
    nazwaPliku = paste0("dane wyskalowane/", rodzajEgzaminu, "-kontekstowe.RData")
    save(list = paste0(skrotEgzaminu, "Kontekstowe"), file = nazwaPliku)
    message(" zapisano do pliku: ", nazwaPliku,
            format(Sys.time(), "\n (%Y.%m.%d, %H:%M:%S)"))
    pliki = append(pliki, nazwaPliku)
  }

  if (czyZamykacSrc) {
    rozlacz(src)
  }
  invisible(pliki)
}
