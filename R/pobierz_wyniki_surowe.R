#' @title Pobieranie surowych wynikow egzaminu
#' @description
#' Funkcja pobiera surowe wyniki egzaminu i zapisuje je na dysku w postaci
#' plików RData.
#' \itemize{
#'   \item{Funkcja sprawdzi, czy w aktywnym katalogu istnieje katalog
#'         \code{dane surowe}. Jeśli tak, zapisze tam pliki, jeśli nie, najpierw
#'         go utworzy, a potem zapisze tam pliki.}
#'   \item{Dane dotyczące wyników z poszczególnych lat zapisane zostaną
#'         w oddzielnych plikach, o nazwach postaci \code{nazwa egzaminu rok.RData}.}
#'   \item{Każdy plik w formacie RData będzie zawierał ramki danych z wynikami
#'         poszczególnych części egzaminu.
#'         \itemize{
#'           \item{Nazwy ramek danych odpowiadają prefiksom tych części egzaminu
#'                 w bazie, w tablic\code{sl_czesci_egzaminow}.}
#'           \item{Ich wewnętrzna struktura jest taka sama, jak struktura
#'                 wartości zwracanej przez funkcję pobierz_czesc_egzaminu()
#'                 z pakietu ZPD.}
#'           \item{Mają przypisane klasy (oprócz \code{data.frame}):
#'                 \code{czescEgzaminu} i \code{wynikiSurowe}.}
#'           \item{Mają przypisany atrybut \code{dataPobrania}, zawierający
#'                 datę w formie ciągu znaków postaci rrrr-mm-dd.}
#'         }
#'        }
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
#' @param src NULL połączenie z bazą danych IBE zwracane przez funkcję
#' \code{\link[ZPD]{polacz}}; pozwala posłużyć się połączeniem o wyższych niż
#' domyślne prawach dostępu, co ma znaczenie dla zakresu pobieranych danych
#' kontekstowych
#' @return lista z nazwami zapisanych plików (niewidocznie)
#' @import ZPD
#' @export
pobierz_wyniki_surowe = function(rodzajEgzaminu, lata = NULL, nadpisz = FALSE,
                                 daneKontekstowe = TRUE, src = NULL) {
  stopifnot(
    is.character(rodzajEgzaminu), length(rodzajEgzaminu) == 1,
    all(rodzajEgzaminu %in% c("sprawdzian", "egzamin gimnazjalny", "matura")),
    is.numeric(lata) | is.null(lata), length(lata) > 0 | is.null(lata),
    all(as.integer(lata) == lata),
    all(nadpisz %in% c(TRUE, FALSE)), length(nadpisz) == 1,
    all(daneKontekstowe %in% c(TRUE, FALSE)), length(daneKontekstowe) == 1,
    is.src(src) | is.null(src)
  )
  czyZamykacSrc = FALSE
  if (is.null(src)) {
    src = polacz()
    czyZamykacSrc = TRUE
  }
  if (is.null(lata)) {
    lata = pobierz_testy(src) %>%
      filter_(~rodzaj_egzaminu == rodzajEgzaminu) %>%
      select_(.dots = ~rok) %>%
      distinct %>%
      collect %>%
      as.list %>%
      unlist %>%
      sort
  }
  skrotEgzaminu = sub("e", "g", substr(rodzajEgzaminu, 1, 1))

  # sprawdzanie, co jest na dysku
  if (!dir.exists("dane surowe")) {
    dir.create("dane surowe")
    message("Utworzono katalog 'dane surowe' w aktywnym katalogu:\n'", getwd(),"'\n")
  }
  czyPobrane = file.exists(paste0("dane surowe/", rodzajEgzaminu, " ", lata, ".RData"))
  if (any(czyPobrane)) {
    message("Istnieją już zapisane pliki z wynikami tego egzaminu z lat: ",
            paste0(lata[czyPobrane], collapse = ", "), ".\n",
            ifelse(nadpisz,
                   "Zostaną one nadpisane nowo pobranymi danymi.\n",
                   "Dane z tych lat nie zostaną pobrane.\n"))
    if (!nadpisz) {
      lata = lata[!czyPobrane]
    }
  }
  if (daneKontekstowe) {
    if (file.exists(paste0("dane surowe/", rodzajEgzaminu,
                           "- kontekstowe.RData"))) {
      message("Istnieje już zapisany plik z danymi kontekstowymi. ",
              "Zostanie on nadpisany nowymi danymi.\n")
    }
  }

  message(rodzajEgzaminu, "\n", format(Sys.time(), "(%Y.%m.%d, %H:%M:%S)"))
  # pobieranie i zapis wyników
  for (i in lata) {
    message("\nRok ", i, ":")
    if ((rodzajEgzaminu == "sprawdzian" & i < 2003) |
        (rodzajEgzaminu == "egzamin gimnazjalny" & i < 2006) |
        (rodzajEgzaminu == "matura" & i < 2010)) {
      czyEwd = FALSE
    } else {
      czyEwd = TRUE
    }
    czesciEgzaminu = pobierz_testy(src) %>%
      filter_(~rodzaj_egzaminu == rodzajEgzaminu, ~rok == i, ~czy_egzamin) %>%
      select_(.dots = list(~czesc_egzaminu, ~prefiks)) %>%
      collect %>%
      unique
    for (j in 1:nrow(czesciEgzaminu)) {
      if (czesciEgzaminu$czesc_egzaminu[j] != "") {
        message("  ", czesciEgzaminu$czesc_egzaminu[j])
      }
      temp = pobierz_wyniki_egzaminu(src, rodzajEgzaminu,
                                     czesciEgzaminu$czesc_egzaminu[j],
                                     i, czyEwd) %>%
        collect
      class(temp) = append(class(temp), c("wynikiSurowe", "czescEgzaminu"))
      attributes(temp)$dataPobrania = Sys.time()
      assign(czesciEgzaminu$prefiks[j], temp)
      rm(temp)
    }
    nazwaPliku = paste0("dane surowe/", rodzajEgzaminu, " ", i, ".RData")
    save(list = czesciEgzaminu$prefiks, file = nazwaPliku)
    message(" zapisano do pliku: ", nazwaPliku,
            format(Sys.time(), "\n (%Y.%m.%d, %H:%M:%S)"))
  }
  pliki = paste0("dane surowe/", rodzajEgzaminu, " ", lata, ".RData")
  # pobieranie i zapis danych kontekstowych
  if (daneKontekstowe) {
    message("\nDane o uczniach i szkołach:")
    temp = pobierz_dane_kontekstowe(src, rodzajEgzaminu)
    class(temp) = append(class(temp), "daneKontekstowe")
    attributes(temp)$dataPobrania = Sys.time()
    assign(paste0(skrotEgzaminu, "Kontekstowe"), temp)
    rm(temp)
    nazwaPliku = paste0("dane surowe/", rodzajEgzaminu, "-kontekstowe.RData")
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
