#' @title Formaty zmiennych
#' @description
#' Funkcja zmienia formaty zmiennych z takich jakie zwracane są przy pobieraniu
#' z bazy na takie, które są bardziej użyteczne przy modelowaniu w R.
#' @details
#' Zmienne opisujące specyfikę szkoły, posiadanie zaświadczenia o dysleksji
#' i bycie laureatem zamieniane są na factory o poziomach "nie" i "tak".
#' Płeć zamieniana jest na factor o poziomach "mężczyzna" i "kobieta".
#' Jeśli w danych występują zmienne \code{rok_g} i \code{rok_s} lub \code{rok_m}
#' i \code{rok_g}, tworzona jest zmienna \code{wydl}, opisująca długość toku
#' kształcenia (dokładnie wydłużenie względem standardowego). Jest ona factorem
#' o poziomach "0", "1", i ew. dalszych.
#' @param x data frame zawierający dane
#' @param usunPusteKolumny opcjonalnie wartość logiczna - czy usunąć puste
#' kolumny opisujące grupę w skalowaniu, błędy standardowe oraz kolumnę 'nr_pv',
#' jeśli skalowanie nie generowało PV?
#' @return data frame
formaty_zmiennych_baza_na_ewd = function(x, usunPusteKolumny = TRUE) {
  stopifnot(all(usunPusteKolumny %in% c(TRUE, FALSE)),
            length(usunPusteKolumny) == 1)
  names(x) = sub("laur_", "laureat_", names(x))
  names(x) = sub("dysl_", "dysleksja_", names(x))
  for (i in grep("^(dla_doroslych|specjalna|przyszpitalna|publiczna)_", names(x))) {
    x[, i] = factor(as.numeric(x[[i]]), levels = 0:1, labels = c("nie", "tak"))
  }
  for (i in grep("^(dysleksja|laureat)_", names(x))) {
    x[, i] = factor(as.numeric(x[[i]]), levels = 0:1, labels = c("nie", "tak"))
  }
  x$plec = factor(x$plec, levels = c("m", "k"), labels = c("mężczyzna", "kobieta"))
  if (all(c("rok_g", "rok_s") %in% names(x))) {
    x = cbind(x, wydl = factor(x$rok_g - 3 - x$rok_s))
  } else if (all(c("rok_m", "rok_g") %in% names(x))) {
    x = cbind(x, wydl = factor(x$rok_m - ifelse(x$typ_szkoly_m == "T", 4, 3) - x$rok_g))
  }
  # ew. usuwanie pustych kolumn
  if (usunPusteKolumny) {
    if (all(unique(x$nr_pv) %in% -1)) {
      x = x[, names(x) != "nr_pv"]
    }
    maskaGrupaBS = grep("^(grupa|bs)_", names(x))
    pusteGrupaBS = unlist(lapply(x[, maskaGrupaBS],
                                 function(x) {return(all(x == "" | is.na(x)))}))
    x = x[, -maskaGrupaBS[pusteGrupaBS]]
  }
  # porządki z kolejnością
  skrotyEgz = sub("^rok_", "", names(x)[grep("^rok_", names(x))])
  egzWe = c("s", "g", "m")[min(unlist(list("s" = 1, "g" = 2, "m" = 3)[skrotyEgz]))]
  egzWy = c("s", "g", "m")[max(unlist(list("s" = 1, "g" = 2, "m" = 3)[skrotyEgz]))]
  kolejnoscKolumn = c(
    grep("^id_(obserwacji|cke)$|^plec$", names(x)),  # id ucznia i płeć
    # egzamin "na wyjściu"
    setdiff(grep(paste0("_", egzWy, "$|^pomin_szkole$|^lu_wszyscy$"), names(x)),
            grep("^(laureat|wiek|dysleksja|klasa|id_testu)", names(x))),  # dane szkoły
    grep(paste0("^(klasa|wiek|dysleksja)_", egzWy), names(x)),  # dane ucznia
    grep(paste0("^laureat_", egzWy), names(x)),  # bycie laureatem
    grep(paste0("^", egzWy, ".*", "_suma$"), names(x)),  # wyniki surowe
    grep(paste0("^", egzWy, ".*", "_norm$"), names(x)),  # wyniki znorm.
    setdiff(grep(paste0("^", egzWy, ".*", "_irt$"), names(x)),  # wyniki wyskal.
            grep("^grupa", names(x))),
    grep(paste0("^bs_", egzWy, ".*", "_irt$"), names(x)),  # bł. std. wyn. wysk.
    grep(paste0("^grupa_", egzWy, ".*", "_irt$"), names(x)),  # grupa w skalowaniu
    # egzamin "na wejściu"
    setdiff(grep(paste0("_", egzWe, "$"), names(x)),
            grep("^(laureat|wiek|dysleksja|klasa|id_testu)", names(x))),  # dane szkoły
    grep(paste0("^(klasa|wiek|dysleksja)_", egzWe), names(x)),  # dane ucznia
    grep(paste0("^laureat_", egzWe), names(x)),  # bycie laureatem
    grep(paste0("^", egzWe, ".*", "_suma$"), names(x)),  # wyniki surowe
    grep(paste0("^", egzWe, ".*", "_norm$"), names(x)),  # wyniki znorm.
    setdiff(grep(paste0("^", egzWe, ".*", "_irt$"), names(x)),  # wyniki wyskal.
            grep("^grupa", names(x))),
    grep(paste0("^bs_", egzWe, ".*", "_irt$"), names(x)),  # bł. std. wyn. wysk.
    grep(paste0("^grupa_", egzWe, ".*", "_irt$"), names(x)),  # grupa w skalowaniu
    # id_testu (zbiory z danych surowych)
    grep(paste0("^id_testu_", egzWe, ".*", "_irt$"), names(x))  # grupa w skalowaniu
  )
  x = x[, c(kolejnoscKolumn, setdiff(1:ncol(x), kolejnoscKolumn))]
  # koniec
  return(x)
}

