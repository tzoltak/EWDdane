% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pobierz_dane_szkol.r
\name{pobierz_dane_szkol}
\alias{pobierz_dane_szkol}
\title{Pobieranie danych o szkolach}
\usage{
pobierz_dane_szkol(
  lata,
  typySzkol = NULL,
  idOke = FALSE,
  daneAdresowe = FALSE,
  src = NULL
)
}
\arguments{
\item{lata}{wektor liczb całkowitych - lata, których mają dotyczyć dane (dla
każdej szkoły zwrócone zostaną tylko najświeższe dane w ramach tego okresu)}

\item{typySzkol}{opcjonalny wektor tekstowy z typami szkół, które mają zostać
zwrócone (lub NULL - zwraca informacje o wszystkich szkołach)}

\item{idOke}{wartość logiczna (domyślnie FALSE) - czy dołączać kody OKE szkół?}

\item{daneAdresowe}{wartość logiczna (domyślnie FALSE) - czy dołączać nazwę
i dane adresowe?}

\item{src}{NULL połączenie z bazą danych IBE zwracane przez funkcję
\code{\link[ZPD]{polacz}}; pozwala posłużyć się niestandardowymi parametrami
połączenia}
}
\value{
data frame
}
\description{
Funkcja pobiera z bazy dane o szkołach - o ich typie i specyfice, nazwie,
adresowe i o lokalizacji.
}
