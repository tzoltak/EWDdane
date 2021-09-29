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
#' @param src NULL połączenie z bazą danych IBE zwracane przez funkcję
#' \code{\link[ZPD]{polacz}}. Jeśli nie podane, podjęta zostanie próba
#' automatycznego nawiązania połączenia.
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
#' @importFrom stats setNames
#' @import ZPD
#' @export
pobierz_parametry_skalowania = function(skala, skalowanie = NULL,
                                        doPrezentacji = NA,
                                        parametryzacja = "baza", src = NULL) {
  stopifnot(is.character(skala) | is.numeric(skala), length(skala) == 1,
            is.null(skalowanie) | is.character(skalowanie) | is.numeric(skalowanie),
            is.logical(doPrezentacji), length(doPrezentacji) == 1,
            is.character(parametryzacja), length(parametryzacja) == 1,
            is.src(src) | is.null(src))
  stopifnot(parametryzacja %in% c("baza", "mplus"))
  if (!is.null(skalowanie)) {
    stopifnot(length(skalowanie) == 1)
  }
  if (is.null(src)) {
    src = ZPD::polacz()
  }

  # szukamy skal
  skale = pobierz_skale(src, doPrezentacji = NA, czyKtt = FALSE) %>%
    select(-.data$id_testu, -.data$grupa) %>%
    collect(n = Inf) %>%
    distinct()
  if (!is.na(doPrezentacji)) {
    skale = filter(skale, .data$skala_do_prezentacji == doPrezentacji)
  }
  if (is.character(skala)) {
    skale = filter(skale, grepl(skala, .data$opis_skali))
    lSkal = length(unique(skale$id_skali))
    if (lSkal == 0) {
      stop("Nie udało się znaleźć skali o opisie pasującym do wyrażenia '",
           skala, "'.")
    }
    message("Znaleziono ", lSkal, " skal(e/ę), których/ej opis pasuje do wyrażenia '",
           skala, "':\n ", paste0(sort(unique(skale$id_skali)), collapse = ", "), ".")
  } else {
    skale = filter(skale, .data$id_skali == skala)
    if (nrow(skale) == 0) {
      stop("Nie udało się znaleźć skali o id skali = ", skala, ".")
    }
  }

  # teraz trzeba wybrać skalowanie
  skale = suppressWarnings(
    arrange(skale, .data$id_skali, desc(.data$skalowanie_do_prezentacji), desc(.data$skalowanie)) %>%
    group_by(.data$id_skali) %>%
      mutate(max_skalowanie = max(.data$skalowanie, na.rm = TRUE)) %>%
      mutate(brak_skalowan = all(is.na(.data$skalowanie))) %>%
      mutate(max_skalowanie = ifelse(.data$brak_skalowan, 0, .data$max_skalowanie)) %>%
      ungroup()
  )

  if (is.character(skalowanie)) {
    skale = mutate(skale, wybrane_skalowanie = grepl(local(skalowanie), .data$opis_skalowania))
  } else if (is.numeric(skalowanie)) {
    skale = mutate(skale, wybrane_skalowanie = local(skalowanie) %in% .data$skalowanie)
  } else {
    skale = mutate(skale, wybrane_skalowanie = !.data$brak_skalowan)
    if (!is.na(doPrezentacji)) {
      skale = skale %>%
        mutate(wybrane_skalowanie = .data$skalowanie_do_prezentacji == doPrezentacji)
    }
  }

  # pobieranie parametrów
  skaleZeSkalowaniem = skale %>%
    filter(.data$wybrane_skalowanie) %>%
    select(.data$id_skali, .data$rodzaj_egzaminu, .data$skalowanie, .data$opis_skali, .data$max_skalowanie) %>%
    distinct()
  parametry = suppressMessages(
    pobierz_parametry(src) %>%
      inner_join(select(skaleZeSkalowaniem, -.data$max_skalowanie), copy = TRUE) %>%
      collect(n = Inf)
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
      select(.data$id_skali) %>%
      distinct() %>%
      mutate(ma_parametry = TRUE)
  )
  if (ncol(skaleZeSkalowaniem) == 0) {
    # jeśli w wyniku semi_joina wypadają wszystkie wiersze, to wyparowują też kolumny
    skaleZeSkalowaniem = matrix(nrow = 0, ncol = 2, dimnames = list(NULL, c("id_skali", "ma_parametry"))) %>%
      as_tibble()
  }

  # przekształcanie parametrów
  parametry = parametry %>%
    group_by(.data$id_skali, .data$rodzaj_egzaminu, .data$opis_skali, .data$skalowanie) %>%
    nest(parametry = all_of(c("grupa", "kryterium", "parametr_uwagi",
                              "parametr", "model", "wartosc", "bs"))) %>%
    ungroup()  # jeśli jest tylko jedno skalowanie, to zostało właśnie zgubione grupowanie po nim
  if (parametryzacja == "mplus") {
    parametry = parametry %>%
      group_by(.data$id_skali, .data$rodzaj_egzaminu, .data$opis_skali, .data$skalowanie) %>%
      do(parametry = zmien_na_mplus(.data)) %>%
      ungroup()
  }

  # skale, dla których nic mądrego nie znaleźliśmy (i informowanie o nich użytkownika)
  skale = suppressMessages(
    skale %>%
      group_by(.data$id_skali, .data$rodzaj_egzaminu, .data$opis_skali) %>%
      summarise(
        brak_skalowan = .data$brak_skalowan[1],
        jest_wybrane_skalowanie = any(.data$wybrane_skalowanie),
        max_skalowanie = .data$max_skalowanie[1],
      ) %>%
      ungroup() %>%
      mutate(max_skalowanie = ifelse(is.na(.data$max_skalowanie), 0, .data$max_skalowanie)) %>%
      full_join(skaleZeSkalowaniem)
  )
  if (any(skale$brak_skalowan)) {
    warning(
      "Skala/e o id_skali: ",
      paste0(skale$id_skali[skale$brak_skalowan], collapse = ", "),
      " nie ma(ją) zarejestrowanych żadnych skalowań."
    )
  }
  if (with(skale, any(!jest_wybrane_skalowanie & !brak_skalowan))) {
    warning(
      "Skala/e o id_skali: ",
      with(skale, paste0(id_skali[!jest_wybrane_skalowanie & !brak_skalowan], collapse = ", ")),
      " nie ma(ją) zarejestrowanych żadnych skalowań, które spełniają podane kryteria."
    )
  }
  if (with(skale, any(is.na(ma_parametry) & jest_wybrane_skalowanie & !brak_skalowan))) {
    warning(
      "Żadne ze skalowań, które spełniają podane kryteria, skal(i) o id_skali: ",
      with(skale, paste0(id_skali[is.na(ma_parametry) & jest_wybrane_skalowanie & !brak_skalowan], collapse = ", ")),
      " nie ma(ją) zapisanych w bazie wartości parametrów modelu."
    )
  }
  skale = skale %>%
    filter(!.data$jest_wybrane_skalowanie | is.na(.data$ma_parametry)) %>%
    mutate(
      skalowanie = .data$max_skalowanie + 1L,
      parametry = vector(mode = "list", length = n())
    ) %>%
    select(.data$id_skali, .data$rodzaj_egzaminu, .data$opis_skali, .data$skalowanie, .data$parametry)

  # koniec
  return(
    bind_rows(skale, parametry) %>%
    arrange(.data$id_skali, .data$skalowanie)
  )
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
#' @importFrom stats setNames
#' @import dplyr
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
  grm   = filter(x, .data$model %in% "GRM")
  binarne = filter(x, .data$model %in% "2PL")
  grupowe  = filter(x, !is.na(.data$grupa) & .data$parametr != "r EAP") %>%
    anti_join(grm) %>%
    anti_join(binarne)

  if ((nrow(grm) + nrow(binarne) + nrow(grupowe)) !=
      nrow(filter(x, .data$parametr != "r EAP"))) {
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
    a = with(
      filter(binarne, .data$parametr == "a"),
      tibble(
        typ = rep("by", length(wartosc)),
        zmienna1 = rep(nazwaKonstruktu, length(wartosc)),
        zmienna2 = kryterium,
        wartosc = wartosc,
        "S.E." = bs
      )
    ),
    b = with(
      filter(binarne, .data$parametr == "trudność"),
      tibble(
        typ = rep("threshold", length(wartosc)),
        zmienna1 = kryterium,
        zmienna2 = rep("1", length(wartosc)),
        wartosc = wartosc,
        "S.E." = bs
      )
    )
  )
  binarne$b = suppressMessages(
    full_join(
      binarne$b,
      with(
        binarne$a,
        tibble(zmienna1 = zmienna2, a = wartosc)
      )
    )
  )
  binarne$b = within(
    binarne$b,
    {
      wartosc = get("wartosc") * get("a")
      S.E. = get("S.E.") * get("a")
    }
  )
  binarne$b = select(binarne$b, -.data$a)
  binarne = bind_rows(binarne)

  #GRM
  maska = grm$parametr %in% c("a", "trudność", paste0("b", 1:9))
  if (!all(maska)) {
    stop("Niepoprawne rodzaje parametrów dla modelu GRM:\n  ",
         paste(grm$parametr[maska], collapse = "\n - "))
  }
  grm = list(
    a = with(
      filter(grm, .data$parametr == "a"),
      tibble(
        typ = rep("by", length(wartosc)),
        zmienna1 = rep(nazwaKonstruktu, length(wartosc)),
        zmienna2 = kryterium,
        wartosc = wartosc,
        "S.E." = bs
      )
    ),
    b = with(
      filter(grm, substr(.data$parametr, 1, 1) == "b"),
      tibble(
        typ = rep("threshold", length(wartosc)),
        zmienna1 = kryterium,
        zmienna2 = sub("^b([[:digit:]]+)$", "\\1", parametr),
        wartosc = wartosc,
        "S.E." = bs
      )
    ),
    g = with(
      filter(grm, .data$parametr == "trudność"),
      tibble(
        zmienna1 = kryterium,
        trudnosc = wartosc
      )
    )
  )
  grm$b = suppressMessages(
    full_join(
      grm$b,
      with(
        grm$a,
        tibble(zmienna1 = zmienna2, a = wartosc)
      )
    ) %>%
    full_join(grm$g)
  )
  grm$b = within(
    grm$b,
    {
      wartosc = get("wartosc") + get("trudnosc") * get("a")
      S.E. = get("S.E.") * get("a")
    }
  )
  grm$b = select(grm$b, -.data$a, -.data$trudnosc)
  grm = bind_rows(grm[c("a", "b")])

  # parametry grupowe
  grupowe = bind_rows(
    sr = with(
      filter(grupowe, .data$parametr == "group_mean"),
      tibble(
        typ = paste0("mean", ifelse(grupa == "", "", paste0(".gr.", grupa))),
        zmienna1 = nazwaKonstruktu,
        zmienna2 = NA,
        wartosc = wartosc,
        "S.E." = bs
      )
    ),
    war = with(
      filter(grupowe, .data$parametr == "group_sd"),
      tibble(
        typ = paste0("variance", ifelse(grupa == "", "", paste0(".gr.", grupa))),
        zmienna1 = nazwaKonstruktu,
        zmienna2 = NA,
        wartosc = wartosc^2,
        "S.E." = bs
      )
    )
  )

  wynik = bind_rows(binarne, grm) %>%
    arrange(.data$typ, .data$zmienna1, .data$zmienna2) %>%
    bind_rows(grupowe)
  # r EAP i param. standaryzacji
  rEAP = filter(x, .data$parametr == "r EAP")
  if (nrow(rEAP) > 0) {
    attributes(wynik)$"r EAP" = rEAP[ c("grupa", "wartosc")]
  }
  std = filter(x, .data$parametr %in% c("std_mean", "std_sd"))
  if (nrow(std) > 0) {
    stdSr = filter(std, .data$parametr == "std_mean") %>%
      select(.data$grupa, .data$wartosc) %>%
      setNames(c("grupa", "sr"))
    stdOs = filter(std, .data$parametr == "std_sd") %>%
      select(.data$grupa, .data$wartosc) %>%
      setNames(c("grupa", "os"))
    attributes(wynik)$"paramStd" = full_join(stdSr, stdOs)
  }

  return(wynik)
}
