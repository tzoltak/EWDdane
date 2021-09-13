#' @title Pobieranie danych kontekstowych o uczniach
#' @description
#' Pobiera z bazy danych zmaterializowany zbiór danych zawierający wszelkie
#' infromacje kontekstowe potrzebne przy obliczaniu EWD.
#'
#' Jeden wiersz zwracanej ramki danych opisuje pierwsze/ostatnie (patrz niżej)
#' przystąpienie ucznia do wskazanego rodzaju egzaminu, a informacje dotyczące
#' poszczególnych części egzaminu (na chwilę obecną tylko informacja o byciu
#' laureatem z danego przedmiotu) znajdują się w kolumnach prefiksowanych
#' zgodnie z tablicą sl_czesci_egzaminow w bazie.
#'
#' Jeśli informacje o szkole, klasie, numerze w dzienniku lub dysleksji nie są
#' dla danego ucznia zgodne w różnych częściach egzaminu, wtedy dana informacja
#' (szkoła, klasa, dysleksja) zastępowana jest brakiem danych.
#'
#' W zależności od poziomu uprawnień połączenia z bazą danych funkcja
#' automatycznie pobiera lub nie informacje o kodzie klasy i numerze ucznia w
#' dzienniku oraz o jego wieku, który podawany jest jako liczba ukończonych
#' miesięcy życia w dniu pisania najwcześniej przeprowadzonej części egzaminu
#' (w danym roku).
#'
#' Dane pobierane są w postaci zmaterializowanej z powodów wydajnościowych -
#' zapytanie realizujące całą niezbędną funkcjonalnośc po stronie bazy danych
#' wykonywałoby się wieki.
#' @param src połączenie z bazą danych IBE zwracane przez funkcję
#' \code{\link[ZPD]{polacz}}
#' @param rodzajEgzaminu ciąg znaków - rodzaj egzaminu
#' @return data.frame
#' @details
#' Oprócz innych informacji funkcja zwraca również zmienne logiczne opisujące,
#' czy dane osoby należą do "wzorcowych" populacji, wykorzystywanych do
#' estymacji parametrów modeli skalowania i/lub modeli EWD. Są to zmienne
#' \code{populacja_we} i \code{populacja_wy}. Członkowie populacji "wzorcowych"
#' definiowani są jako zdający:
#' \itemize{
#'   \item{Którzy uczyli się w szkołach dla młodzieży, nie będących szkołami
#'         specjalnymi, przyszpitalnymi, itp.}
#'   \item{Którzy podchodzili do egzaminu:
#'         \itemize{
#'           \item{Po raz pierwszy - przy skalowaniu i odnośnie wyników
#'                 "na wyjściu" w modelach EWD - zmienna \code{populacja_wy}.}
#'           \item{Po raz ostatni (odnotowany) - odnośnie wyników "na wejściu"
#'                 w modelach EWD - zmienna \code{populacja_we}.}
#'         }
#'        }
#'   \item{Dodatkowo, tylko dla zmiennej \code{populacja_wy}, a więc
#'         w odniesieniu do skalowania i wyników "na wyjściu" w modelach EWD,
#'         populacja "wzorcowa" zawężana jest do zdających:
#'         \itemize{
#'           \item{Którzy należą do "głównej" kohorty wiekowej dla danego
#'                 egzaminu, lub kohort o rok lub dwa lata starszych lub
#'                 młodszych, tzn.:
#'                 \itemize{
#'                   \item{Dla ucznów SP w wieku 124-183 miesięcy.}
#'                   \item{Dla uczniów gimn. w wieku 160-219 miesięcy.}
#'                   \item{Dla uczniów LO w wieku 196-255 miesięcy.}
#'                   \item{Dla uczniów T w wieku 208-267 miesięcy.}
#'                 }
#'                 Przy tym informacja o wieku nie jest uwzględniana w przypadku
#'                 sprawdzianu z lat 2002-2004, egzaminu gimnazjalnego z lat
#'                 2002-2007 i matury z lat 2002-2008, bo informacje o nim są
#'                 w tych latach niedostępne.
#'           }
#'           \item{W przypadku matury brane są pod uwagę wyłącznie osoby,
#'                 których kod klasy nie jest liczbą. Wynika to z faktu, że
#'                 liczbowe oznaczenia klas często wykorzystywane są przez OKE
#'                 dla oznaczenia osób, które nie były uczniami danej szkoły,
#'                 ale pisały w niej maturę.}
#'         }
#'        }
#' }
#' Dodatkowe uwagi:
#' \itemize{
#'   \item{Informacje o wieku i klasie są użyteczne tylko wtedy, jeśli
#'         przekazane do funkcji połączenie z bazą posiada poziom uprawnień,
#'         który umożliwia ich pobranie.}
#'   \item{Przed przystąpieniem do skalowania na populacji "wzorcowej" konieczne
#'         jest jeszcze usunięcie z danych z wynikami surowymi egzaminów
#'         punktacji za zadania przypisanej laureatom. Musi to jednak zostać
#'         zrobinone oddzielnie dla każdej części egzaminu (na podstawie
#'         zwróconych przez tę funkcję informacji o byciu laureatem).}
#' }
#' @import dplyr
#' @import tidyr
#' @import ZPD
#' @export
pobierz_dane_kontekstowe = function(src, rodzajEgzaminu) {
  stopifnot(
    is.src(src),
    is.vector(rodzajEgzaminu), is.character(rodzajEgzaminu),
    length(rodzajEgzaminu) == 1, all(!is.na(rodzajEgzaminu))
  )

  # sprawdzanie poziomu uprawnień
  daneOsobowe = FALSE
  try({
    tmp = tbl(src, sql("SELECT id_obserwacji FROM dane_osobowe.obserwacje LIMIT 1"))
    daneOsobowe = TRUE
  }, silent = TRUE)

  # definicje agregatów na poziomie {uczeń, rodzajEgzaminu, rok}
  dotsSummarize = function(dane) {
    dane %>% summarize(
        data       = min(.data$data_testu, na.rm = T),
        id_szkoly  = min(.data$id_szkoly, na.rm = T),
        dysleksja  = all(.data$dysleksja, na.rm = T),
        id_szkoly2 = max(.data$id_szkoly, na.rm = T),
        dysleksja2 = any(.data$dysleksja, na.rm = T),
        klasa      = min(.data$klasa, na.rm = T),
        kod_u      = min(.data$kod_u, na.rm = T),
        klasa2     = max(.data$klasa, na.rm = T),
        kod_u2     = max(.data$kod_u, na.rm = T)
      )
  }
  dotsMutate = function(dane) {
    dane %>% mutate(
      id_szkoly = ifelse(.data$id_szkoly == .data$id_szkoly2, .data$id_szkoly, NA),
      dysleksja = ifelse(.data$dysleksja == .data$dysleksja2, .data$dysleksja, NA),
      klasa  = ifelse(.data$klasa == .data$klasa2, .data$klasa, NA),
      kod_u  = ifelse(.data$kod_u == .data$kod_u2, .data$kod_u, NA)
    )
  }
  dotsSelect = '^(id_szkoly2|dysleksja2|klasa2|kod_u2)$'

  # pobranie danych na poziomie {uczeń, rodzajEgzaminu, czescEgzaminu, rok} i agregacja do {uczeń, rok}
  message(" Pobieranie danych o przystępowaniu do egzaminów.", format(Sys.time(), " (%Y.%m.%d, %H:%M:%S)"))
  testy = suppressMessages(
    pobierz_testy(src) %>%
      filter(.data$dane_ewd == TRUE, .data$rodzaj_egzaminu == rodzajEgzaminu, .data$czy_egzamin == TRUE) %>%
      select(.data$id_testu, .data$prefiks, .data$data_testu)
  )
  if (select(testy, .data$id_testu) %>% collect() %>% nrow() == 0) {
    stop("Podano rodzaj egzaminu, który nie występuje w bazie.")
  }
  uczniowieTesty = suppressMessages(
    pobierz_dane_uczniowie_testy(src, daneOsobowe = daneOsobowe) %>%
      inner_join(testy) %>%
      select(-.data$pop_podejscie, -.data$oke, -.data$zrodlo, -.data$id_testu) %>%
      collect(n = Inf)
  )
  if (daneOsobowe == FALSE) {
    uczniowieTesty = uczniowieTesty %>%
      mutate(
        klasa = 'brak danych',
        kod_u = -999L
      )
  }

  # pobieranie informacji o szkołach
  message(" Pobieranie informacji o szkołach.",
          format(Sys.time(), " (%Y.%m.%d, %H:%M:%S)"))
  szkoly = pobierz_szkoly(src) %>%
    select(
      .data$id_szkoly, .data$rok, .data$typ_szkoly, .data$publiczna, .data$specjalna,
      .data$dla_doroslych, .data$przyszpitalna, .data$artystyczna
    ) %>%
    collect(n = Inf)

  # pobieranie informacji o obserwacjach
  message(" Pobieranie informacji o uczniach.", format(Sys.time(), " (%Y.%m.%d, %H:%M:%S)"))
  obserwacje = pobierz_uczniow(src, daneOsobowe = daneOsobowe) %>%
    select(-.data$id_cke) %>%
    distinct() %>%
    collect(n = Inf)

  # pobieranie informacji o pierwszych przystąpieniach
  message(" Pobieranie informacji o pierwszych i ostatnich przystąpieniach do egzaminu.", format(Sys.time(), " (%Y.%m.%d, %H:%M:%S)"))
  pierwsze = filtruj_przystapienia(
    src, pierwsze = TRUE,
    rodzajEgzaminu = rodzajEgzaminu,
    czescEgzaminu = NULL, czyEwd = TRUE
  ) %>%
    collect(n = Inf) %>%
    select(-.data$rodzaj_egzaminu, -.data$dane_ewd) %>%
    mutate(pierwsze = TRUE)

  # pobieranie informacji o ostatnich przystąpieniach
  ostatnie = filtruj_przystapienia(
    src, pierwsze = FALSE,
    rodzajEgzaminu = rodzajEgzaminu,
    czescEgzaminu = NULL, czyEwd = TRUE
  ) %>%
    collect(n = Inf) %>%
    select(-.data$rodzaj_egzaminu, -.data$dane_ewd) %>%
    mutate(ostatnie = TRUE)

  # obróbka danych indywidualnych
  message(" Konwersja danych na format krótszy.",
          format(Sys.time(), " (%Y.%m.%d, %H:%M:%S)"))
  dane = suppressWarnings(
    uczniowieTesty %>%
      group_by(.data$id_obserwacji, .data$rok) %>%
      dotsSummarize() %>%
      ungroup() %>%
      dotsMutate() %>%
      select(-matches(dotsSelect))
  )

  # konwersja informacji o byciu laureatem do postaci szerokiej
  message(" Obrabianie informacji o byciu laureatem.", format(Sys.time(), " (%Y.%m.%d, %H:%M:%S)"))
  uczniowieTesty = uczniowieTesty %>%
    mutate(prefiks = paste0('laur_', .data$prefiks)) %>%
    select("id_obserwacji", "rok", "prefiks", "laureat") %>%
    pivot_wider(names_from = "prefiks", values_from = "laureat")

  # złączenie i usunięcie zbędnych danych
  dane = suppressMessages(inner_join(dane, uczniowieTesty))
  rm(uczniowieTesty)

  # dołączenie informacji o szkołach
  message(" Dołączanie informacji o szkołach.", format(Sys.time(), " (%Y.%m.%d, %H:%M:%S)"))
  dane = suppressMessages(inner_join(dane, szkoly))
  rm(szkoly)

  # dołączanie informacji o obserwacjach
  message(" Dołączanie informacji o uczniach.", format(Sys.time(), " (%Y.%m.%d, %H:%M:%S)"))
  dane = suppressMessages(inner_join(dane, obserwacje))
  rm(obserwacje)

  # oznaczamy pierwsze przystąpienia
  message(" Oznaczanie pierwszych i ostatnich przystąpień do egzaminu.", format(Sys.time(), " (%Y.%m.%d, %H:%M:%S)"))
  dane = suppressMessages(left_join(dane, pierwsze))
  rm(pierwsze)

  # oznaczamy ostatnie przystąpienia
  dane = suppressMessages(left_join(dane, ostatnie))
  rm(ostatnie)

  # ew. obliczenie wieku w miesiącach
  if (daneOsobowe) {
    message(" Obliczanie wieku.", format(Sys.time(), " (%Y.%m.%d, %H:%M:%S)"))
    dane = dane %>%
      mutate(
        wiek = as.numeric(substr(.data$data, 1, 4)) * 12 +
               as.numeric(substr(.data$data, 6, 7)) -
               as.numeric(substr(.data$data_ur, 1, 4)) * 12 -
               as.numeric(substr(.data$data_ur, 6, 7))
      )
  }
  dane = dane %>% select(-.data$data)

  # wygenerowanie zmiennej określającej populację
  message(" Generowanie zmiennnych określających populacje wzorcowe.",
          format(Sys.time(), " (%Y.%m.%d, %H:%M:%S)"))
  dane = dane %>%
    mutate(
      populacja_we = .data$ostatnie %in% TRUE,
      populacja_wy = .data$pierwsze %in% TRUE & .data$id_szkoly > 0
    )
  if (rodzajEgzaminu == "sprawdzian") {
    dane = dane %>%
      mutate(
        pomin_szkole = !(.data$dla_doroslych %in% FALSE)
      )
  } else {
    dane = dane %>%
      mutate(
        pomin_szkole = !(.data$specjalna %in% FALSE & .data$dla_doroslych %in% FALSE & .data$przyszpitalna %in% FALSE)
      )
  }
  if (daneOsobowe) {
    if (rodzajEgzaminu == "sprawdzian") {
      wiekWzor = 153.5
      rokSprawdzWiek = 2005
    } else if (rodzajEgzaminu == "egzamin gimnazjalny") {
      wiekWzor = 189.5
      rokSprawdzWiek = 2008
    } else {
      wiekWzor = 225.5
      rokSprawdzWiek = 2010
    }
    dane = dane %>%
      mutate(
        populacja_wy = .data$populacja_wy &
          ifelse(
            .data$rok >= rokSprawdzWiek,
            (.data$wiek - ifelse(.data$typ_szkoly %in% "T", 12, 0)) %in% seq(wiekWzor - 29.5, wiekWzor + 29.5, 1),
            TRUE
          )
      )
    if (rodzajEgzaminu == "matura") {
      dane = dane %>%
        mutate(
          populacja_wy = .data$populacja_wy & grepl("^[^[:digit:]]+$", .data$klasa)
        )
    }
  } else {
    warning("Ze względu na zbyt niski poziom uprawnień dostępu do bazy ",
            "niemożliwe było wykorzystanie informacji o wieku i kodzie klasy ",
            "przy tworzeniu zmiennych 'populacja_we' i 'populacja_wy'.")
  }

  # obsługa bycia laureatem w egzaminie gimnazjalnym
  if (rodzajEgzaminu == "egzamin gimnazjalny") {
    if (all(c("laur_gh", "laur_gh_h", "laur_gh_p") %in% names(dane))) {
      dane = dane %>% mutate(
        laur_gh = ifelse(.data$rok < 2012, .data$laur_gh, .data$laur_gh_h & .data$laur_gh_p)
      )
    }
    if (all(c("laur_gm", "laur_gm_m", "laur_gm_p") %in% names(dane))) {
      dane = dane %>% mutate(
        laur_gm = ifelse(.data$rok < 2012, .data$laur_gm, .data$laur_gm_m & .data$laur_gm_p)
      )
    }
  }
  message(" Zakończono pobieranie danych kontekstowych.", format(Sys.time(), " (%Y.%m.%d, %H:%M:%S)"))

  # koniec
  class(dane) = c("daneKontekstowe", class(dane))
  attributes(dane)$dataPobrania = Sys.time()
  return(dane)
}
