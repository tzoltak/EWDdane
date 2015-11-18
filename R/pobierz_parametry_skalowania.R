#' @title Pobieranie parametrow skalowan
#' @description
#' Funkcja pobiera parametry skalowań o podanej nazwie skali i numerze testu
#' oraz których opis skalowań spełnia podane wyrażenie regularne.
#' @param skala id_skali (liczba naturalna) lub ciąg znaków z wyrażeniem
#' regularnym, do którego ma pasować opis skali
#' @param skalowanie opcjonalnie numer skalowania (liczba naturalna) lub ciąg
#' znaków z wyrażeniem regularnym, do którego ma pasować opis skalowania
#' @param doPrezentacji wartość logiczna - czy zwracać skale/skalowania
#' oznaczone jako do prezentacji (\code{TRUE} - tylko oznaczone jako do
#' prezentacji, \code{FALSE} - tylko oznaczone jako nie do prezentacji,
#' \code{NA} - wszystkie, bez względu na to, czy są do prezentacji, czy nie)
#' @param parametryzacja parametr określający format zwracanego wyniku. Domyślna
#' wartość to 'baza'. Inna możliwa wartość to 'mplus'.
#' @return data frame (data table) o kolumnach:
#' \itemize{
#'    \item{id_skali;}
#'    \item{rodzaj_egzaminu;}
#'    \item{opis_skali;}
#'    \item{skalowaie;}
#'    \item{parametry.}
#' }
#' Ostatnie kolumna zawiera data frame'y z wartościami parametrów skalowań.
#' W zależności od wartości argumentu \code{parametryzacja} albo w formacie
#' zwracanym przez funkcję \code{\link[ZPD]{pobierz_parametry}}, albo w formacie
#' wykorzystywanym przez funkcję \code{\link[EWDskalowanie]{skaluj}}.
#' @import ZPD
#' @export
pobierz_parametry_skalowania = function(skala, skalowanie = NULL,
                                        doPrezentacji = NA,
                                        parametryzacja = "baza") {
  stopifnot(is.character(skala) | is.numeric(skala), length(skala) == 1,
            is.null(skalowanie) | is.character(skalowanie) | is.numeric(skalowanie),
            is.logical(doPrezentacji), length(doPrezentacji) == 1,
            is.character(parametryzacja), length(parametryzacja) == 1)
  stopifnot(parametryzacja %in% c("baza", "mplus"))
  if (!is.null(skalowanie)) {
    stopifnot(length(skalowanie) == 1)
  }

  # szukamy skal
  skale = pobierz_skale(polacz(), doPrezentacji = NA, czyKtt = FALSE) %>%
    select_(~-id_testu, ~-grupa) %>%
    collect(skale) %>%
    distinct()
  if (!is.na(doPrezentacji)) {
    skale = filter_(skale, ~skala_do_prezentacji == doPrezentacji)
  }
  if (is.character(skala)) {
    skale = filter_(skale, ~grepl(skala, opis_skali))
    lSkal = length(unique(skale$id_skali))
    if (lSkal == 0) {
      stop("Nie udało się znaleźć skali o opisie pasującym do wyrażenia '",
           skala, "'.")
    }
    message("Znaleziono ", lSkal, " skal(e/ę), których/ej opis pasuje do wyrażenia '",
           skala, "':\n ", paste0(sort(unique(skale$id_skali)), collapse = ", "), ".")
  } else {
    skale = filter_(skale, ~id_skali == skala)
    if (nrow(skale) == 0) {
      stop("Nie udało się znaleźć skali o id skali = ", skala, ".")
    }
  }
  idSkal = sort(unique(skale$id_skali))

  # teraz trzeba wybrać skalowanie
  skale = suppressWarnings(
    arrange_(skale, ~id_skali, ~desc(skalowanie_do_prezentacji),
             ~desc(skalowanie)) %>%
    group_by_(~id_skali) %>%
      mutate_(.dots = setNames(list(~max(skalowanie, na.rm = TRUE)),
                               "max_skalowanie")) %>%
      mutate_(.dots = setNames(list(~all(is.na(skalowanie))),
                               "brak_skalowan")) %>%
      ungroup()
  )

  if (is.character(skalowanie)) {
    skale = mutate_(skale, .dots = setNames(list(~grepl(skalowanie, opis_skalowania)),
                                            "wybrane_skalowanie"))
  } else if (is.numeric(skalowanie)) {
    nrSkalowania = skalowanie
    skale = mutate_(skale, .dots = setNames(list(~skalowanie %in% nrSkalowania),
                                            "wybrane_skalowanie"))
  } else {
    skale = mutate_(skale, .dots = setNames(list(~!brak_skalowan),
                                            "wybrane_skalowanie"))
    if (!is.na(doPrezentacji)) {
      skale =
        mutate_(skale,
                .dots = setNames(list(~skalowanie_do_prezentacji == doPrezentacji),
                                 "wybrane_skalowanie"))
    }
  }

  # pobieranie parametrów
  skaleZeSkalowaniem = filter_(skale, ~wybrane_skalowanie) %>%
    select_(~id_skali, ~rodzaj_egzaminu, ~skalowanie, ~opis_skali,
            ~max_skalowanie) %>%
    distinct()
  parametry = suppressMessages(
    pobierz_parametry(polacz()) %>%
      inner_join(select_(skaleZeSkalowaniem, ~-max_skalowanie), copy = TRUE) %>%
      collect()
  )
  if (ncol(parametry) == 0) {
    # jeśli w wyniku semi_joina wypadają wszystkie wiersze, to wyparowują też kolumny
    parametry = matrix(nrow = 0, ncol = 4,
                       dimnames = list(NULL, c("id_skali", "rodzaj_egzaminu",
                                               "opis_skali", "skalowanie"))) %>%
      as.data.frame()
    mode(parametry$rodzaj_egzaminu) = "character"
    mode(parametry$opis_skali) = "character"
  }

  skaleZeSkalowaniem = suppressMessages(
    semi_join(skaleZeSkalowaniem, parametry) %>%
      select_(~id_skali) %>%
      distinct() %>%
      mutate_(.dots = setNames(list(~TRUE), "ma_parametry"))
  )
  if (ncol(skaleZeSkalowaniem) == 0) {
    # jeśli w wyniku semi_joina wypadają wszystkie wiersze, to wyparowują też kolumny
    skaleZeSkalowaniem = matrix(nrow = 0, ncol = 2,
                       dimnames = list(NULL, c("id_skali", "ma_parametry"))) %>%
      as.data.frame()
  }

  # przekształcanie parametrów
  parametry = group_by_(parametry, ~id_skali, ~rodzaj_egzaminu, ~opis_skali,
                        ~skalowanie) %>%  # dramatycznie zawikłana składnia pod dplyr-a
    summarise_(.dots = setNames(list(~list(data.frame(grupa = grupa[1:n()],
                                                      kryterium = kryterium[1:n()],
                                                      uwagi = parametr_uwagi[1:n()],
                                                      parametr = parametr[1:n()],
                                                      model = model[1:n()],
                                                      wartosc = wartosc[1:n()],
                                                      bs = bs[1:n()],
                                                      stringsAsFactors = FALSE))),
                                "parametry")) %>%
    ungroup()  # jeśli jest tylko jedno skalowanie, to zostało właśnie zgubione grupowanie po nim
  if (parametryzacja == "mplus") {
    parametry = group_by_(parametry, ~id_skali, ~rodzaj_egzaminu, ~opis_skali,
                          ~skalowanie) %>%
      do_(.dots = setNames(list(~zmien_na_mplus(.)), "parametry")) %>%
      ungroup()
  }
  #parametry = select_(parametry, ~-rodzaj_egzaminu, ~-opis_skali)

  # skale, dla których nic mądrego nie znaleźliśmy (i informowanie o nich użytkownika)
  skale = suppressMessages(
    group_by_(skale, ~id_skali, ~rodzaj_egzaminu, ~opis_skali) %>%
      summarise_(.dots = setNames(list(~brak_skalowan[1], ~any(wybrane_skalowanie),
                                       ~max_skalowanie[1]),
                                  c("brak_skalowan", "jest_wybrane_skalowanie",
                                    "max_skalowanie"))) %>%
      ungroup() %>%
      mutate_(.dots = setNames(list(~ifelse(is.na(max_skalowanie),
                                            0, max_skalowanie)),
                               "max_skalowanie")) %>%
      full_join(skaleZeSkalowaniem)
  )
  if (any(skale$brak_skalowan)) {
    warning("Skala/e o id_skali: ",
            paste0(skale$id_skali[skale$brak_skalowan], collapse = ", "),
            " nie ma(ją) zarejestrowanych żadnych skalowań.")
  }
  if (with(skale, any(!jest_wybrane_skalowanie & !brak_skalowan))) {
    warning("Skala/e o id_skali: ",
            with(skale, paste0(id_skali[!jest_wybrane_skalowanie &
                                          !brak_skalowan],
                               collapse = ", ")),
            " nie ma(ją) zarejestrowanych żadnych skalowań, które spełniają podane kryteria.")
  }
  if (with(skale, any(is.na(ma_parametry) & jest_wybrane_skalowanie & !brak_skalowan))) {
    warning("Żadne ze skalowań, które spełniają podane kryteria, skal(i) o id_skali: ",
            with(skale, paste0(id_skali[is.na(ma_parametry) &
                                          jest_wybrane_skalowanie & !brak_skalowan],
                               collapse = ", ")),
            " nie ma(ją) zapisanych w bazie wartości parametrów modelu.")
  }
  skale = filter_(skale, ~!jest_wybrane_skalowanie | is.na(ma_parametry)) %>%
    mutate_(.dots = setNames(list(~max_skalowanie + 1, ~NA),
                             c("skalowanie", "parametry"))) %>%
    select_(~id_skali, ~rodzaj_egzaminu, ~opis_skali, ~skalowanie, ~parametry)

  # koniec
  return(bind_rows(skale, parametry) %>%
           arrange_(~id_skali, ~skalowanie))
}
#' @title Zmiana tablicy do formatu funkcji skaluj()
#' @description
#' Funkcja przekształca parametryzację z bazy (klasyczne, jednowymiarowe IRT)
#' na parametryzację stosowaną przez Mplusa.
#' @param x data frame (data table) w formacie takim, jaki zwraca funkcja
#' \code{\link[ZPD]{pobierz_parametry}} z pakietu \code{ZPD}
#' @return
#' Funkcja zwraca ramkę danych, która jest zgodna z postacią ramek zwracanych
#' przez funkcję \code{\link[EWDskalowanie]{skaluj}}.
zmien_na_mplus = function(x) {
  stopifnot(is.data.frame(x))

  idSkali = x$id_skali[1]
  skalowanie = x$skalowanie[1]
  if ("opis_skali" %in% names(x)) {
    nazwaKonstruktu = gsub("^ewd;([^;]+);.*$", "\\1", x$opis_skali[1])
  } else {
    nazwaKonstruktu = "theta"
  }
  x = x$parametry[[1]]
  maska = is.na(x$kryterium) & !is.na(x$uwagi)
  x$kryterium[maska] = x$uwagi[maska]
  grm   = filter_(x, ~model %in% "GRM")
  binarne = filter_(x, ~model %in% "2PL")
  grupowe  = filter_(x, ~!is.na(grupa) & parametr != "r EAP")

  if ((nrow(grm) + nrow(binarne) + nrow(grupowe)) !=
      nrow(filter_(x, ~parametr != "r EAP"))) {
    stop("Przy konwersji parametrów na format 'mplus' obsługowane są wyłącznie ",
         "zadania 2PL lub SGR oraz ew. średnie i odchylenia standardowe ",
         "konstruktu w ramach grup.")
  }

  # zadania binarne
  maska = binarne$parametr %in% c("a", "trudność")
  if (!all(maska)) {
    stop("Niepoprawne rodzaje parametrów dla modelu 2PL:\n  ",
         paste(binarne$parametr[maska], collapse = "\n - "))
  }
  binarne = list(
    a = with(filter_(binarne, ~parametr == "a"),
             data.frame(typ = rep("by", length(wartosc)),
                        zmienna1 = rep(nazwaKonstruktu, length(wartosc)),
                        zmienna2 = kryterium,
                        wartosc = wartosc,
                        "S.E." = bs,
                        stringsAsFactors = FALSE)),
    b = with(filter_(binarne, ~parametr == "trudność"),
             data.frame(typ = rep("threshold", length(wartosc)),
                        zmienna1 = kryterium,
                        zmienna2 = rep("1", length(wartosc)),
                        wartosc = wartosc,
                        "S.E." = bs,
                        stringsAsFactors = FALSE))
  )
  binarne$b = suppressMessages(
    full_join(binarne$b, with(binarne$a, data.frame(zmienna1 = zmienna2, a = wartosc,
                                                stringsAsFactors = FALSE)))
  )
  binarne$b = within(binarne$b, {
    wartosc = get("wartosc") * get("a")
    S.E. = get("S.E.") * get("a")
  })
  binarne$b = select_(binarne$b, ~-a)
  binarne = bind_rows(binarne)

  #GRM
  maska = grm$parametr %in% c("a", "trudność", paste0("b", 1:9))
  if (!all(maska)) {
    stop("Niepoprawne rodzaje parametrów dla modelu GRM:\n  ",
         paste(grm$parametr[maska], collapse = "\n - "))
  }
  grm = list(
    a = with(filter_(grm, ~parametr == "a"),
             data.frame(typ = rep("by", length(wartosc)),
                        zmienna1 = rep(nazwaKonstruktu, length(wartosc)),
                        zmienna2 = kryterium,
                        wartosc = wartosc,
                        "S.E." = bs,
                        stringsAsFactors = FALSE)),
    b = with(filter_(grm, ~substr(parametr, 1, 1) == "b"),
             data.frame(typ = rep("threshold", length(wartosc)),
                        zmienna1 = kryterium,
                        zmienna2 = sub("^b([[:digit:]]+)$", "\\1", parametr),
                        wartosc = wartosc,
                        "S.E." = bs,
                        stringsAsFactors = FALSE)),
    g = with(filter_(grm, ~parametr == "trudność"),
             data.frame(zmienna1 = kryterium,
                        trudnosc = wartosc,
                        stringsAsFactors = FALSE))
  )
  grm$b = suppressMessages(
    full_join(grm$b, with(grm$a, data.frame(zmienna1 = zmienna2, a = wartosc,
                                            stringsAsFactors = FALSE))) %>%
      full_join(grm$g)
  )
  grm$b = within(grm$b, {
    wartosc = get("wartosc") + get("trudnosc") * get("a")
    S.E. = get("S.E.") * get("a")
  })
  grm$b = select_(grm$b, ~-a, ~-trudnosc)
  grm = bind_rows(grm[c("a", "b")])

  # parametry grupowe
  grupowe = bind_rows(
    sr = with(filter_(grupowe, ~parametr == "group_mean"),
             data.frame(typ = paste0("mean", ifelse(grupa == "", "",
                                                    paste0(".gr.", grupa))),
                        zmienna1 = nazwaKonstruktu,
                        zmienna2 = NA,
                        wartosc = wartosc,
                        "S.E." = bs,
                        stringsAsFactors = FALSE)),
    war = with(filter_(grupowe, ~parametr == "group_sd"),
               data.frame(typ = paste0("variance", ifelse(grupa == "", "",
                                                          paste0(".gr.", grupa))),
                          zmienna1 = nazwaKonstruktu,
                          zmienna2 = NA,
                          wartosc = wartosc^2,
                          "S.E." = bs,
                          stringsAsFactors = FALSE))
  )
  grupowe$typ

  wynik = bind_rows(binarne, grm) %>%
    arrange_(~typ, ~zmienna1, ~zmienna2) %>%
    bind_rows(grupowe)
  # r EAP
  rEAP = filter_(x, ~parametr == "r EAP")
  if (nrow(rEAP) > 0) {
    attributes(wynik)$"r EAP" = rEAP[ c("grupa", "wartosc")]
  }

  return(wynik)
}
