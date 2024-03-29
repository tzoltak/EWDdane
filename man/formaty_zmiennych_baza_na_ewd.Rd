% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formaty_zmiennych.R
\name{formaty_zmiennych_baza_na_ewd}
\alias{formaty_zmiennych_baza_na_ewd}
\title{Formaty zmiennych}
\usage{
formaty_zmiennych_baza_na_ewd(x, usunPusteKolumny = TRUE)
}
\arguments{
\item{x}{data frame zawierający dane}

\item{usunPusteKolumny}{opcjonalnie wartość logiczna - czy usunąć puste
kolumny opisujące grupę w skalowaniu, błędy standardowe oraz kolumnę 'nr_pv',
jeśli skalowanie nie generowało PV?}
}
\value{
data frame
}
\description{
Funkcja zmienia formaty zmiennych z takich jakie zwracane są przy pobieraniu
z bazy na takie, które są bardziej użyteczne przy modelowaniu w R.
}
\details{
Zmienne opisujące specyfikę szkoły, posiadanie zaświadczenia o dysleksji
i bycie laureatem zamieniane są na factory o poziomach "nie" i "tak".
Płeć zamieniana jest na factor o poziomach "mężczyzna" i "kobieta".
Jeśli w danych występują zmienne \code{rok_g} i \code{rok_s} lub \code{rok_m}
i \code{rok_g}, tworzona jest zmienna \code{wydl}, opisująca długość toku
kształcenia (dokładnie wydłużenie względem standardowego). Jest ona factorem
o poziomach "0", "1", i ew. dalszych.
}
