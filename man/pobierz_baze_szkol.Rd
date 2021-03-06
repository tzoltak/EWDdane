% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pobierz_baze_szkol.R
\name{pobierz_baze_szkol}
\alias{pobierz_baze_szkol}
\title{Aktualizacja bazy danych szkol}
\usage{
pobierz_baze_szkol(typySzkol, zapis = NULL, src = NULL)
}
\arguments{
\item{typySzkol}{opcjonalny wektor tekstowy z typami szkół, które mają zostać
zwrócone (lub NULL - zwraca informacje o wszystkich szkołach)}

\item{zapis}{opcjonalnie nazwa pliku, do którego zostaną zapisane pobrane dane
(w formacie csv)}

\item{src}{NULL połączenie z bazą danych IBE zwracane przez funkcję
\code{\link[ZPD]{polacz}}; pozwala posłużyć się niestandardowymi parametrami
połączenia}
}
\value{
data frame (niewidocznie)
}
\description{
Funkcja pobiera dane o szkołach, potrzebne do dokonania aktualizacji bazy
szkół (wykonuje zrzut bazy szkół).
}
