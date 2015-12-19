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
    daneKontekstowe = group_by_(daneKontekstowe, ~id_szkoly) %>%
      mutate_(lu_wszyscy = ~n())
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
#' @title Funkcje pomocnicze przy przygotowywaniu danych do wyliczania modeli EWD
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
      skale = rbind(skale, attributes(daneRok)$skale)
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
#' @title Funkcje pomocnicze przy przygotowywaniu danych do wyliczania modeli EWD
#' @description Funkcja wczytuje wyniki surowe egzaminu z pojedynczego pliku.
#' @param nazwaPliku ciąg znaków - nazwa pliku z surowymi wynikami egzaminu
#' @import ZPD
#' @return data table
wczytaj_wyniki_surowe = function(nazwaPliku) {
  stopifnot(is.character(nazwaPliku), length(nazwaPliku) == 1)
  stopifnot(file.exists(nazwaPliku))

  obiekty = load(nazwaPliku)
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

  skale = pobierz_skale(polacz(), doPrezentacji = NA) %>%
    filter_(~opis_skalowania == "normalizacja ekwikwantylowa EWD") %>%
    collect()
  skaleTesty = list()
  normy = list()
  # idziemy po obiektach
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
        temp = normalizuj(temp, src = polacz(), idSkali = idSkali,
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
#' @title Funkcje pomocnicze przy przygotowywaniu danych do wyliczania modeli EWD
#' @description Funkcja wczytuje wyniki surowe egzaminu z pojedynczego pliku.
#' @param nazwaPliku ciąg znaków - nazwa pliku z surowymi wynikami egzaminu
#' @details
#' Jeśli plik dla jakiejś skali zawiera wyniki więcej niż jednego skalowania,
#' to zwrócone zostaną wyniki tylko jednego z nich, przy czym piwerwszeństwo
#' będą mieć skalowania oznaczone jako do prezentacji, a w dalszej kolejności
#' te, których data skalowania jest najpóżniejsza.
#' @import ZPD
#' @import reshape2
#' @return data table
wczytaj_wyniki_wyskalowane = function(nazwaPliku) {
  stopifnot(is.character(nazwaPliku), length(nazwaPliku) == 1)
  stopifnot(file.exists(nazwaPliku))

  skale = data.frame()
  obiekty = load(nazwaPliku)
  for (i in obiekty) {
    oszacowania = get(i)
    # konwersja grup na liczby, żeby mogły być przetwarzana melt/dcast z oszacowaniami
    oszacowania$grupa = factor(oszacowania$grupa)
    grupy = levels(oszacowania$grupa)
    oszacowania$grupa = as.numeric(oszacowania$grupa)
    rm(i)
    if ("wynikiWyskalowane" %in% class(oszacowania)) {
      skale = bind_rows(
        skale,
        attributes(oszacowania)$skale %>%
          arrange_(~id_skali, ~desc(skalowanie_do_prezentacji),
                   ~desc(data_skalowania)) %>%
          group_by_(~id_skali) %>%
          mutate_(.dots = setNames(list(~1:n()), "priorytet")) %>%
          ungroup() %>%
          filter_(~priorytet == 1) %>%
          select_(~-priorytet) %>%
          mutate_(.dots = setNames(list(~sub("^ewd;([^;]+);.*$", "\\1_irt",
                                             opis_skali)),
                                   "zmienna"))
      )
      oszacowania = suppressMessages(
        inner_join(oszacowania, select_(skale, ~id_skali, ~zmienna)) %>%
          select_(~-id_skali, ~-skalowanie) %>%
          melt(measure.vars = c("wynik", "bs", "grupa"), variable.name = "co",
               value.name = "wartosc") %>%
          dcast(id_obserwacji + rok + nr_pv ~ co + zmienna,
                value.var = "wartosc")
      )
      names(oszacowania) = sub("wynik_", "", names(oszacowania))
      # grupy znów na ciągi znaków
      maskaZmGrupy = grep("^grupa_", names(oszacowania))
      oszacowania[, maskaZmGrupy] = lapply(oszacowania[, maskaZmGrupy],
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
#' @title Funkcje pomocnicze przy przygotowywaniu danych do wyliczania modeli EWD
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
#' @title Funkcje pomocnicze przy przygotowywaniu danych do wyliczania modeli EWD
#' @description Funkcja wczytuje dane z plików .RData z wynikami surowymi
#' egzaminu, i przygotowuje zestawienie opisujące, którzy zdający przystępowali
#' do których części egzaminu.
#' @param nazwyPlikow wektor ciągów znaków - nazwy plików z surowymi wynikami
#' egzaminu
#' @import dplyr
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
        temp = get(j) %>%
          select_(~id_obserwacji, ~rok) %>%
          mutate_(.dots = setNames(list(~TRUE),
                                   paste0("zdawal_", j)))
        przystepowanie[[i]] = suppressMessages(
          full_join(przystepowanie[[i]], temp))
      }
      rm(list = j)
    }
  }
  przystepowanie = rbind_all(przystepowanie)
  maska = is.na(przystepowanie[, grep("^zdawal_", names(przystepowanie))])
  przystepowanie[, grep("^zdawal_", names(przystepowanie))][maska] = FALSE

  return(przystepowanie)
}
