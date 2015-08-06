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
#' @return data frame
formaty_zmiennych_baza_na_ewd = function(x) {
  for (i in grep("^dla_doroslych_|^specjalna_|^przyszpitalna_", names(x))) {
    x[, i] = factor(x[, i], levels=0:1, labels=c("nie", "tak"))
  }
  for (i in grep("^dysleksja_|^laureat_", names(x))) {
    x[, i] = factor(x[, i], levels=0:1, labels=c("nie", "tak"))
  }
  x$plec=factor(x$plec, levels=c("m", "k"), labels=c("mężczyzna", "kobieta"))
  if (all(c("rok_g", "rok_s") %in% names(x))) {
    x = cbind(x, wydl = factor(x$rok_g - 3 - x$rok_s))
  } else if (all(c("rok_m", "rok_g") %in% names(x))) {
    x = cbind(x, wydl = factor(x$rok_m - ifelse(x$typ_szkoly_m == "T", 4, 3) - x$rok_g))
  }
  return(x)
}

