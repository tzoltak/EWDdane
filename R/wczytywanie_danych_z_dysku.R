#' @title Funkcje pomocnicze przy przygotowywaniu danych do obliczania modeli EWD
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
#' @import dplyr
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
    daneKontekstowe = group_by(daneKontekstowe, .data$id_szkoly) %>%
      mutate(lu_wszyscy = n()) %>%
      ungroup()
    maska = paste0("^(plec|id|rok|laur|dysleksja|klasa|publiczna|specjalna)|",
                   "^(typ_szkoly|przyszpitalna|dla_doroslych|artystyczna|wiek)|",
                   "^(pomin_szkole|lu_)")
  } else {
    daneKontekstowe = subset(daneKontekstowe, get("populacja_we") %in% TRUE)
    maska = paste0("^(id|rok|laur|dysleksja)")
  }
  daneKontekstowe = daneKontekstowe[, grep(maska, names(daneKontekstowe))]

  maska = !grepl("^(id_obserwacji|plec|pomin_szkole|lu_wszyscy)$|^laur_",
                 names(daneKontekstowe))
  names(daneKontekstowe)[maska] = paste0(names(daneKontekstowe)[maska],
                                         skrotEgzaminu)
  return(daneKontekstowe)
}
#' @title Funkcje pomocnicze przy przygotowywaniu danych do obliczania modeli EWD
#' @description Funkcja wczytuje wyniki egzaminu (surowe lub wyskalowane)
#' z podanych plików, łączy je w jeden zbiór i ewentualnie łączy z podanymi
#' danymi kontekstowymi.
#'
#' Reguły wyboru, które wyniki zostaną zwrócone w sytuacji, gdy wykorzystywane
#' są wyniki wyskalowane i dla jakiejś skali dostępne są wyniki z kilku różńych
#' skalowań, opisane są w pomocy do funkcji
#' \code{\link{wczytaj_wyniki_wyskalowane}}.
#' @param nazwyPlikow wektor ciągów znaków - nazwy plików z surowymi wynikami
#' egzaminu
#' @param daneKontekstowe opcjonalnie (acz typowo) data frame z danymi
#' kontekstowymi, typowo wynik wywołania funkcji
#' \code{\link{wczytaj_dane_kontekstowe}}
#' @return data table
wczytaj_wyniki_egzaminu = function(nazwyPlikow, daneKontekstowe = NULL) {
  stopifnot(is.character(nazwyPlikow), length(nazwyPlikow) > 0,
            is.null(daneKontekstowe) | is.data.frame(daneKontekstowe))
  stopifnot(all(file.exists(nazwyPlikow)))

  if (!is.null(daneKontekstowe)) {
    skrotEgzaminu = names(daneKontekstowe)[grepl("^rok_", names(daneKontekstowe))]
    skrotEgzaminu = sub("^rok", "", skrotEgzaminu)
  } else {
    skrotEgzaminu = ""
  }
  skaleTesty = list()
  normy = list()
  skale = data.frame()
  for (i in nazwyPlikow) {
    obiekty = load(i)
    klasy = vector(mode = "character", length = 0)
    for (j in obiekty) {
      klasy = c(klasy, class(get(j)))
    }
    klasy = unique(klasy)
    # Trochę się obciążymy czasowo rypaniem dysk->RAM, ale nie będziemy obciążać
    # pamięci trzymaniem w RAMie kopii tych samych obiektów.
    rm(list = obiekty)
    # samo wczytywanie
    if (all(c("wynikiSurowe", "wynikiWyskalowane") %in% klasy)) {
      warning("Nie udało się wczytać danych z pliku '", i, "', bo znajdują ",
              "się tam zarówno wyniki surowe jak i wyskalowane",
              immediate. = TRUE)
      next
    } else if ("wynikiSurowe" %in% klasy) {
      daneRok = wczytaj_wyniki_surowe(i)
      skaleTesty = c(skaleTesty, attributes(daneRok)$skaleTesty)
      normy = c(normy, attributes(daneRok)$normy)
    } else if ("wynikiWyskalowane" %in% klasy) {
      daneRok = wczytaj_wyniki_wyskalowane(i)
      skale = bind_rows(skale, attributes(daneRok)$skale)
    } else {
      warning("W pliku '", i, "', nie ma ani wyników surowych, ani wyskalowanych",
              immediate. = TRUE)
      next
    }
    if (exists("dane", inherits = FALSE)) {
      dane = bind_rows(dane, daneRok)
    } else {
      dane = daneRok
    }
    rm(daneRok)
  }
  if (exists("dane", inherits = FALSE))
  maska = !grepl("^(id_obserwacji|nr_pv)$|^id_testu|_(suma|norm|irt)$", names(dane))
  names(dane)[maska] = paste0(names(dane)[maska], skrotEgzaminu)
  if (!is.null(daneKontekstowe)) {
    temp = suppressMessages(inner_join(daneKontekstowe, dane))
    if (nrow(temp) > 0) {
      dane = temp
    } else {
      warning("Do danych z pliku '", i, "', nie udało się przyłączyć danych kontekstowych.")
    }
  }
  attributes(dane)$skaleTesty = skaleTesty
  attributes(dane)$normy = normy
  attributes(dane)$skale = skale
  return(dane)
}
#' @title Funkcje pomocnicze przy przygotowywaniu danych do obliczania modeli EWD
#' @description Funkcja wczytuje wyniki surowe egzaminu z pojedynczego pliku.
#' @param nazwaPliku ciąg znaków - nazwa pliku z surowymi wynikami egzaminu
#' @param src NULL połączenie z bazą danych IBE zwracane przez funkcję
#' \code{\link[ZPD]{polacz}}; pozwala posłużyć się niestandardowymi parametrami
#' połączenia
#' @import ZPD
#' @return data table
wczytaj_wyniki_surowe = function(nazwaPliku, src) {
  stopifnot(
    is.character(nazwaPliku), length(nazwaPliku) == 1,
    dplyr::is.src(src) | is.null(src)
  )
  stopifnot(file.exists(nazwaPliku))
  if (is.null(src)) {
    src = ZPD::polacz()
  }

  obiekty = load(nazwaPliku)
  # obsługa części egzaminu gimnazjalnego
  testy = pobierz_testy(src) %>%
    filter(.data$prefiks %in% c("gh", "gm") & .data$is.na(.data$arkusz) & .data$dane_ewd == TRUE) %>%
    collect(n = Inf)
  if (exists("gh_h") & exists("gh_p")) {
    gh = suppressMessages(inner_join(get("gh_h"),
                                     get("gh_p")[, names(get("gh_p")) != "id_testu"]))
    gh$id_testu = filter(testy, .data$prefiks == "gh" & .data$rok == gh$rok[1])$id_testu
    obiekty = c(obiekty, "gh")
  }
  if (exists("gm_m") & exists("gm_p")) {
    gm = suppressMessages(inner_join(get("gm_m"),
                                     get("gm_p")[, names(get("gm_p")) != "id_testu"]))
    gm$id_testu = filter(testy, .data$prefiks == "gm", .data$rok == gm$rok[1])$id_testu
    obiekty = c(obiekty, "gm")
  }

  skale = pobierz_skale(src, doPrezentacji = NA) %>%
    filter(.data$opis_skalowania == "normalizacja ekwikwantylowa EWD") %>%
    collect(n = Inf)
  skaleTesty = list()
  normy = list()
  # idziemy po obiektach
  for (konstrukt in obiekty) {
    flaga = FALSE
    temp = get(konstrukt)
    if (all(c("wynikiSurowe", "czescEgzaminu") %in% class(temp))) {
      flaga = TRUE
      temp = zsumuj_punkty(temp)
      temp = filter(temp, !is.na(.data$wynik))
      # trochę szukania, żeby móc przypisać normy
      maskaSkali = paste0("^ewd;", konstrukt, "R;", temp$rok[1],"$")
      idSkali = filter(skale, grepl(maskaSkali, .data$opis_skali)) %>%
        arrange(.data$skalowanie)
      if (nrow(idSkali) > 0) {
        skaleTesty = c(skaleTesty, list(unique(temp$id_testu)))
        names(skaleTesty)[length(skaleTesty)] = idSkali$id_skali[1]
      }
      grupa = last(filter(idSkali, .data$posiada_normy == TRUE)$grupa, default = NULL)
      skalowanie = last(filter(idSkali, .data$posiada_normy == TRUE)$skalowanie, default = NULL)
      idSkali = last(filter(idSkali, .data$posiada_normy == TRUE)$id_skali, default = NULL)
      if (!is.null(idSkali) & !is.null(skalowanie) & !is.null(grupa)) {
        if (grupa == '') {
          temp = normalizuj(temp, src = src, idSkali = idSkali,
                            skalowanie = skalowanie, grupa = grupa)
          normy[[length(normy) + 1]] = pobierz_normy(src) %>%
            filter(.data$id_skali == idSkali & .data$skalowanie == skalowanie & .data$grupa == grupa) %>%
            as_tibble() %>%
            mutate(rok = .data$temp$rok[1], konstrukt = .data$konstrukt)
          names(normy)[length(normy)] = paste0(konstrukt, "_", temp$rok[1])
        }
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
      if (exists("dane", inherits = FALSE)) {
        dane = suppressMessages(full_join(dane, temp))
      } else {
        dane = temp
      }
    }
  }
  attributes(dane)$skaleTesty = skaleTesty
  attributes(dane)$normy = normy
  return(dane)
}
#' @title Funkcje pomocnicze przy przygotowywaniu danych do obliczania modeli EWD
#' @description Funkcja wczytuje wyniki surowe egzaminu z pojedynczego pliku.
#' @param nazwaPliku ciąg znaków - nazwa pliku z surowymi wynikami egzaminu
#' @details
#' Jeśli plik dla jakiejś skali zawiera wyniki więcej niż jednego skalowania,
#' to zwrócone zostaną wyniki tylko jednego z nich, przy czym pierwszeństwo
#' będą mieć skalowania oznaczone jako do prezentacji, a w dalszej kolejności
#' te, których data skalowania jest najpóżniejsza.
#' @importFrom stats setNames
#' @import ZPD
#' @import dplyr
#' @import tidyr
#' @return data table
wczytaj_wyniki_wyskalowane = function(nazwaPliku) {
  stopifnot(is.character(nazwaPliku), length(nazwaPliku) == 1)
  stopifnot(file.exists(nazwaPliku))

  skale = data.frame()
  obiekty = load(nazwaPliku)
  for (i in obiekty) {
    oszacowania = get(i)
    # konwersja grup na liczby, żeby mogły być przetwarzana pivotami z oszacowaniami
    oszacowania$grupa = factor(oszacowania$grupa)
    grupy = levels(oszacowania$grupa)
    oszacowania$grupa = as.numeric(oszacowania$grupa)
    rm(i)
    if ("wynikiWyskalowane" %in% class(oszacowania)) {
      skale = bind_rows(
        skale,
        attributes(oszacowania)$skale %>%
          arrange(.data$id_skali, desc(.data$skalowanie_do_prezentacji), desc(.data$data_skalowania)) %>%
          group_by(.data$id_skali) %>%
          mutate(priorytet = 1:n()) %>%
          ungroup() %>%
          filter(.data$priorytet == 1) %>%
          select(-"priorytet") %>%
          mutate(zmienna = sub("^ewd;([^;]+);.*$", "\\1_irt", .data$opis_skali))
      )
      oszacowania = suppressMessages(
        inner_join(oszacowania, select(skale, "id_skali", "skalowanie", "zmienna")) %>%
          select(-c("id_skali", "skalowanie")) %>%
          pivot_longer(cols = c("wynik", "bs", "grupa"),
                       names_to = "co", values_to = "wartosc") %>%
          pivot_wider(names_from = c("co", "zmienna"),
                      values_from = "wartosc") %>%
          select("id_obserwacji", "rok", "nr_pv",
                 starts_with("wynik_"), starts_with("bs_"),
                 starts_with("grupa_"))
      )
      names(oszacowania) = sub("wynik_", "", names(oszacowania))
      # grupy znów na ciągi znaków
      maskaZmGrupy = grep("^grupa_", names(oszacowania))
      oszacowania[, maskaZmGrupy] = lapply(oszacowania[, maskaZmGrupy, drop = FALSE],
                                           function(x, grupy) {return(grupy[x])},
                                           grupy = grupy)
      if (exists("dane", inherits = FALSE)) {
        dane = suppressMessages(full_join(dane, oszacowania))
      } else {
        dane = oszacowania
      }
    }
  }
  attributes(dane)$skale = skale
  return(dane)
}
#' @title Funkcje pomocnicze przy przygotowywaniu danych do obliczania modeli EWD
#' @description Funkcja wczytuje wyskalowane wyniki egzaminu z pliku .RData
#' z wynikami skalowania, zapisanego przez funkcje
#' \code{\link[EWDskale]{skaluj_spr}},
#' \code{\link[EWDskale]{skaluj_egz_gimn}} lub
#' \code{\link[EWDskale]{skaluj_matura}}.
#' @param nazwyPlikow wektor ciągów znaków - nazwy plików z surowymi wynikami
#' egzaminu
#' @import dplyr
#' @return data table
wczytaj_wyniki_skalowania = function(nazwyPlikow) {
  stopifnot(is.character(nazwyPlikow), length(nazwyPlikow) > 0)

  oszacowania = data.frame()
  skale = data.frame()
  for (i in nazwyPlikow) {
    obiekty = load(i)
    for (j in obiekty) {
      if ("listaWynikowSkalowania" %in% class(get(j))) {
        rok = suppressMessages(
          as.numeric(sub("^.([[:digit:]]{4})Skalowanie", "\\1", j)))
        temp = lapply(get(j), function(x) {return(x$skalowania_obserwacje)}) %>%
          bind_rows()
        if (!is.na(rok)) {
          cbind(rok = rok)
        }
        oszacowania = bind_rows(oszacowania, temp)
        temp = lapply(get(j), function(x) {return(x$skalowania)}) %>%
          bind_rows()
        skale = bind_rows(skale, temp)
      }
      rm(list = j)
    }
  }
  attributes(oszacowania)$skale = skale
  return(oszacowania)
}
#' @title Funkcje pomocnicze przy przygotowywaniu danych do obliczania modeli EWD
#' @description Funkcja wczytuje dane z plików .RData z wynikami surowymi
#' egzaminu, i przygotowuje zestawienie opisujące, którzy zdający przystępowali
#' do których części egzaminu.
#' @param nazwyPlikow wektor ciągów znaków - nazwy plików z surowymi wynikami
#' egzaminu
#' @importFrom stats setNames
#' @import dplyr
#' @import rlang
#' @return data table
wczytaj_liczbe_przystepujacych = function(nazwyPlikow) {
  stopifnot(is.character(nazwyPlikow), length(nazwyPlikow) > 0)

  przystepowanie = setNames(vector(mode = "list", length = length(nazwyPlikow)),
                            nazwyPlikow)
  for (i in nazwyPlikow) {
    obiekty = load(i)
    przystepowanie[[i]] =
      data.frame(id_obserwacji = vector(mode = "integer", length = 0))
    for (j in obiekty) {
      if (all(c("wynikiSurowe", "czescEgzaminu") %in% class(get(j)))) {
        nazwa = paste0("zdawal_", j)
        temp = get(j) %>%
          select("id_obserwacji", "rok") %>%
          mutate( {{nazwa}} := TRUE)
        przystepowanie[[i]] = suppressMessages(
          full_join(przystepowanie[[i]], temp))
      }
      rm(list = j)
    }
  }
  przystepowanie = bind_rows(przystepowanie)
  maska = is.na(przystepowanie[, grep("^zdawal_", names(przystepowanie))])
  przystepowanie[, grep("^zdawal_", names(przystepowanie))][maska] = FALSE

  return(przystepowanie)
}
