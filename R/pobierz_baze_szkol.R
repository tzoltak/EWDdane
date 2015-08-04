#' @title Aktualizacja bazy danych szkol.
#' @description
#' Funkcja pobiera dane o szkołach, potrzebne do dokonania aktualizacji bazy
#' szkół (wykonuje zrzut bazy szkół).
#' @param typySzkol opcjonalny wektor tekstowy z typami szkół, które mają zostać
#' zwrócone (lub NULL - zwraca informacje o wszystkich szkołach)
#' @param zapis opcjonalnie nazwa pliku, do którego zostaną zapisane pobrane dane
#' (w formacie csv)
#' @return data frame (niewidocznie)
#' @import dplyr
#' @import ZPD
#' @export
pobierz_baze_szkol = function(typySzkol, zapis = NULL) {
  stopifnot(is.character(typySzkol),
            all(typySzkol %in% c("gimn.", "LO", "LP", "T")),
            is.character(zapis) | is.null(zapis)
  )
  if (!is.null(zapis)) {
    stopifnot(length(zapis) == 1)
    if (!grepl("[.](csv|txt|dat)$", zapis)) {
      warning("Plik, do którego mają być zapisane dane ma podane nietypowe rozszerzenie, lub nie ma podanego rozszerzenia.")
    }
  }

  for (typ in typySzkol) {
    daneSzkol = pobierz_dane_szkol(2006:as.numeric(format(Sys.time(), "%Y")),
                                   typ, idOke = FALSE, daneAdresowe = TRUE) %>%
      filter_(~id_szkoly > 0)
    lata = sort(unique(daneSzkol$rok), decreasing = TRUE)
    daneSzkol = select_(daneSzkol, ~-rok, ~-wielkosc_miejscowosci,
                        ~-rodzaj_gminy, ~-matura_miedzynarodowa)
    daneSzkol = within(daneSzkol, {
      oke = c("Wrocław", "Gdańsk", "Kraków", "Poznań", "Łódź", "Kraków",
              "Warszawa", "Wrocław", "Kraków", "Łomża", "Gdańsk", "Jaworzno",
              "Łódź", "Łomża", "Poznań", "Poznań")[floor(get("teryt_szkoly") / 10^4) / 2]
    })
    for (i in lata) {
      kody = pobierz_dane_szkol(i, typ, idOke = TRUE, daneAdresowe = FALSE)
      kody = select_(kody, ~id_szkoly, ~id_szkoly_oke, ~matura_miedzynarodowa)
      if (!(typ %in% c("LO", "LP", "T"))) {
        kody = select_(kody, ~-matura_miedzynarodowa)
      }
      maska = names(kody) != "id_szkoly"
      names(kody)[maska] = paste0(names(kody)[maska], "_", i)
      daneSzkol = suppressMessages(left_join(daneSzkol, kody))
    }
    rm(kody)
    wyniki = bind_rows(get0("wyniki"), daneSzkol)
  }
  maska = unlist(lapply(wyniki, class)) == "logical"
  wyniki[, maska] = lapply(wyniki[, maska], as.numeric)
  if (!is.null(zapis)) {
    write.csv2(wyniki, zapis, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  }
  invisible(wyniki)
}
