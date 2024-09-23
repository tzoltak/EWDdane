#' @title Aktualizacja bazy danych szkol
#' @description
#' Funkcja pobiera dane o szkołach, potrzebne do dokonania aktualizacji bazy
#' szkół (wykonuje zrzut bazy szkół).
#' @param typySzkol opcjonalny wektor tekstowy z typami szkół, które mają zostać
#' zwrócone (lub NULL - zwraca informacje o wszystkich szkołach)
#' @param zapis opcjonalnie nazwa pliku, do którego zostaną zapisane pobrane dane
#' (w formacie csv)
#' @param src NULL połączenie z bazą danych IBE zwracane przez funkcję
#' \code{\link[ZPD]{polacz}}; pozwala posłużyć się niestandardowymi parametrami
#' połączenia
#' @return data frame (niewidocznie)
#' @importFrom utils write.csv2
#' @import dplyr
#' @import ZPD
#' @export
pobierz_baze_szkol = function(typySzkol, zapis = NULL, src = NULL) {
  stopifnot(is.character(typySzkol),
            all(typySzkol %in% c("SP", "gimn.", "LO", "LP", "T", "LOU", "TU")),
            is.character(zapis) | is.null(zapis),
            is.src(src) | is.null(src)
  )
  if (any(typySzkol %in% "SP")) {
    warning("Informacje o SP dotyczące bycia szkołą publiczną, dla dorosłych, specjalną i przyszpitalną mogą być mało wiarygodne.",
            immediate. = TRUE)
  }
  if (!is.null(zapis)) {
    stopifnot(length(zapis) == 1)
    if (!grepl("[.](csv|txt|dat)$", zapis)) {
      warning("Plik, do którego mają być zapisane dane ma podane nietypowe rozszerzenie, lub nie ma podanego rozszerzenia.")
    }
  }

  lata = 2006:as.numeric(format(Sys.time(), "%Y"))
  for (typ in typySzkol) {
    daneSzkol = pobierz_dane_szkol(lata, typ, idOke = FALSE,
                                   daneAdresowe = TRUE, src = src) %>%
      filter(.data$id_szkoly > 0)
    lataTemp = max(daneSzkol$rok):min(daneSzkol$rok)
    daneSzkol = daneSzkol %>%
      select(-c("rok", "wielkosc_miejscowosci", "rodzaj_gminy", "id_rspo",
                "matura_miedzynarodowa"))
    daneSzkol = within(daneSzkol, {
      oke = c("Wrocław", "Gdańsk", "Kraków", "Poznań", "Łódź", "Kraków",
              "Warszawa", "Wrocław", "Kraków", "Łomża", "Gdańsk", "Jaworzno",
              "Łódź", "Łomża", "Poznań", "Poznań")[floor(get("teryt_szkoly") / 10^4) / 2]
    })
    for (i in lataTemp) {
      kody = pobierz_dane_szkol(i, typ, idOke = TRUE, daneAdresowe = FALSE,
                                src = src)
      kody = select(kody, "id_szkoly", "id_szkoly_oke", "id_rspo",
                    "matura_miedzynarodowa")
      if (!(typ %in% c("LO", "LP", "T"))) {
        kody = select(kody, -"matura_miedzynarodowa")
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
