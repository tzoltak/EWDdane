#' @title Pobieranie danych kontekstowych o uczniach.
#' @description
#' Pobiera z bazy danych zmaterializowany zbiór danych zawierający wszelkie
#' infromacje kontekstowe potrzebne przy wyliczaniu EWD.
#' 
#' Jeden wiersz zwracanej ramki danych opisuje pierwsze/ostatnie (patrz niżej)
#' przystąpienie ucznia do wskazanego rodzaju egzaminu, a informacje dotyczące
#' poszczególnych części egzaminu (na chwilę obecną tylko informacja o byciu
#' laureatem z danego przedmiotu) znajdują się w kolumnach prefiksowanych
#' zgodnie z tablicą sl_czesci_egzaminow w bazie.
#' 
#' W zależności od wartości parametru \code{wyjscie} pobierane są informacje o
#' uczniach podchodzących po raz pierwszy (\code{wyjscie == TRUE}) lub po raz
#' ostatni (\code{wyjscie == FALSE}) do wskazanego rodzaju egzaminu.
#' 
#' Jeśli informacje o szkole, klasie, numerze w dzienniku lub dysleksji nie są
#' dla danego ucznia zgodne w różnych częściach egzaminu, wtedy dana informacja
#' (szkoła, klasa, dysleksja) zastępowana jest brakiem danych.
#' 
#' W zależności od poziomu uprawnień połączenia z bazą danych funkcja
#' automatycznie pobiera lub nie informacje o kodzie klasy i numerze ucznia w
#' dzienniku.
#' 
#' Dane pobierane są w postaci zmaterializowanej z powodów wydajnościowych -
#' zapytanie realizujące całą niezbędną funkcjonalnośc po stronie bazy danych
#' wykonywałoby się wieki.
#' @param src połączenie z bazą danych IBE zwracane przez ZPD::polacz()
#' @param rodzajEgzaminu rodzaj egzaminu
#' @return data.frame
#' @import dplyr
#' @import ZPD
#' @export
pobierz_dane_kontekstowe = function(src, rodzajEgzaminu){
  stopifnot(
    is.src(src),
    is.vector(rodzajEgzaminu), is.character(rodzajEgzaminu), length(rodzajEgzaminu) == 1, all(!is.na(rodzajEgzaminu))
  )

  # sprawdzanie poziomu uprawnień
  daneOsobowe = FALSE
  try({
    tmp = tbl(src, sql("SELECT * FROM dane_osobowe.obserwacje LIMIT 1"))
    daneOsobowe = TRUE
  }, silent = TRUE)
  
  # definicje agregatów na poziomie {uczeń, rodzajEgzaminu, rok}
  dotsSummarize = list(
    data       = ~min(data_testu, na.rm = T),
    id_szkoly  = ~min(id_szkoly, na.rm = T), 
    dysleksja  = ~all(dysleksja, na.rm = T), 
    id_szkoly2 = ~max(id_szkoly, na.rm = T), 
    dysleksja2 = ~any(dysleksja, na.rm = T)
  )
  dotsMutate = list(
    id_szkoly = ~ifelse(id_szkoly == id_szkoly2, id_szkoly, NA),
    dysleksja = ~ifelse(dysleksja == dysleksja2, dysleksja, NA)
  )
  dotsSelect = c('-id_szkoly2', '-dysleksja2')
  if(daneOsobowe){
    dotsSummarize = append(dotsSummarize, list(
      klasa      = ~min(klasa,     na.rm = T), 
      kod_u      = ~min(kod_u,     na.rm = T),
      klasa2     = ~max(klasa,     na.rm = T), 
      kod_u2     = ~max(kod_u,     na.rm = T)
    ))
    dotsMutate = append(dotsMutate, list(
      klasa     = ~ifelse(klasa == klasa2, klasa, NA),
      kod_u     = ~ifelse(kod_u == kod_u2, kod_u, NA)
    ))
    dotsSelect = append(dotsSelect, c('-klasa2', '-kod_u2'))
  }
  
  # pobranie danych na poziomie {uczeń, rodzajEgzaminu, czescEgzaminu, rok} i agregacja do {uczeń, rok}
  testy = suppressMessages(
    pobierz_testy(src) %>%
      filter_(~dane_ewd == TRUE, ~rodzaj_egzaminu == rodzajEgzaminu, ~czy_egzamin == TRUE) %>%
      inner_join(tbl(src, sql('SELECT * FROM sl_czesci_egzaminow'))) %>%
      select_('id_testu', 'prefiks', 'data_testu')
  )
  uczniowieTesty = suppressMessages(
    pobierz_dane_uczniowie_testy(src, daneOsobowe = daneOsobowe) %>%
      inner_join(testy) %>%
      select_('-pop_podejscie', '-oke', '-zrodlo', '-id_testu') %>%
      collect()
  )
  dane = suppressWarnings(
    uczniowieTesty %>%
      group_by_('id_obserwacji', 'rok') %>%
      summarize_(.dots = dotsSummarize) %>%
      ungroup() %>%
      mutate_(.dots = dotsMutate) %>%
      select_(.dots = dotsSelect)
  )
  
  # konwersja informacji o byciu laureatem do postaci szerokiej
  laureaci = uczniowieTesty %>%
    mutate_(prefiks = ~paste0('laur_', prefiks)) %>%
    reshape2::dcast(id_obserwacji + rok ~ prefiks, value.var = 'laureat')
  
  # złączenie i usunięcie zbędnych danych
  rm(uczniowieTesty)
  dane = suppressMessages(inner_join(dane, laureaci))
  rm(laureaci)
  
  # dołączenie informacji o szkołach
  szkoly = pobierz_szkoly(src) %>%
    select_('id_szkoly', 'rok', 'typ_szkoly', 'publiczna', 'specjalna', 'dla_doroslych', 'przyszpitalna', 'artystyczna') %>%
    collect()
  dane = suppressMessages(inner_join(dane, szkoly))
  rm(szkoly)
  
  # dołączanie informacji o obserwacjach
  obserwacje = pobierz_uczniow(src, daneOsobowe = daneOsobowe) %>%
    select_('-id_cke') %>%
    collect()
  dane = suppressMessages(inner_join(dane, obserwacje))
  rm(obserwacje)

  # oznaczamy pierwsze przystąpienia
  pierwsze = filtruj_przystapienia(src, pierwsze = TRUE, rodzajEgzaminu = rodzajEgzaminu, czescEgzaminu = NULL, czyEwd = TRUE) %>%
    collect() %>%
    select_('-rodzaj_egzaminu', '-dane_ewd') %>%
    mutate_(pierwsze = TRUE)
  dane = suppressMessages(left_join(dane, pierwsze))
  rm(pierwsze)
  
  # oznaczamy ostatnie przystąpienia
  ostatnie = filtruj_przystapienia(src, pierwsze = FALSE, rodzajEgzaminu = rodzajEgzaminu, czescEgzaminu = NULL, czyEwd = TRUE) %>%
    collect() %>%
    select_('-rodzaj_egzaminu', '-dane_ewd') %>%
    mutate_(ostatnie = TRUE)
  dane = suppressMessages(left_join(dane, ostatnie))
  rm(ostatnie)
  
  # wygenerowanie zmiennej określającej populację
  dane = dane %>%
    mutate_(
      populacja_we = ~publiczna %in% TRUE & specjalna %in% FALSE & dla_doroslych %in% FALSE & przyszpitalna %in% FALSE & ostatnie %in% TRUE,
      populacja_wy = ~publiczna %in% TRUE & specjalna %in% FALSE & dla_doroslych %in% FALSE & przyszpitalna %in% FALSE & pierwsze %in% TRUE
    )
  if(daneOsobowe){
    dane = dane %>%
      mutate(wiek = as.numeric(substr(dane$data, 1, 4)) * 12 + as.numeric(substr(dane$data, 6, 7)) - as.numeric(substr(dane$data_ur, 1, 4)) * 12 - as.numeric(substr(dane$data_ur, 6, 7)))
  }
  dane = dane %>%
    select_('-data')
  
  return(dane)
}