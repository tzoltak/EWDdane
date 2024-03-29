% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pobierz_wartosci_wskaznikow_ewd.r
\name{pobierz_wartosci_wskaznikow_ewd}
\alias{pobierz_wartosci_wskaznikow_ewd}
\title{Pobieranie wartosci wskaznikow EWD}
\usage{
pobierz_wartosci_wskaznikow_ewd(
  typSzkoly,
  lata,
  zapis = NULL,
  jst = NULL,
  idOke = FALSE,
  daneAdresowe = TRUE,
  opisoweNazwy = TRUE,
  lUcznPrzedm = FALSE,
  dodatkoweInfo = FALSE,
  tylkoWskDoPrezentacji = TRUE,
  tylkoWyswietlane = TRUE,
  tylkoNiePomin = TRUE,
  gamma = 0.95,
  fileEncoding = "windows-1250",
  maskaWskazniki = "",
  src = NULL
)
}
\arguments{
\item{typSzkoly}{ciąg znaków: 'gimn.', 'LO' lub 'T'}

\item{lata}{wektor liczb całkowitych - lata, dla których mają zostać pobrane wskaźniki
(wskaźniki identyfikowane są po ostatnim roku w ramach okresu, np. wskaźniki 2013-2011
uzyskamy podając jako argument \code{lata} wartość 2013)}

\item{zapis}{opcjonalnie nazwa pliku, do którego zostaną zapisane pobrane dane
(w formacie csv)}

\item{jst}{opcjonalnie wyrażenie regularne opisujące TERYTy JST, dla których mają
zostać zwrócone wskaźniki lub NULL, jeśli mają zostać zwrócone wartości wskaźników dla
całego kraju}

\item{idOke}{wartość logiczna (domyślnie FALSE) - czy dołączać kody OKE szkół?}

\item{daneAdresowe}{wartość logiczna (domyślnie TRUE) - czy dołączać dane adresowe
szkół?}

\item{opisoweNazwy}{wartość logiczna (domyślnie TRUE) - czy zmieniać nazwy kolumn na
pięknie opisowe?}

\item{lUcznPrzedm}{wartość logiczna (domyślnie FALSE) - czy mają zostać zwrócone
również liczby uczniów zdających poszczególne części egzaminu?}

\item{dodatkoweInfo}{wartość logiczna (domyślnie FALSE) - czy mają zostać zwrócone
pełne charakterystyki wskaźników, wraz z informacjami o korelacji średnich wyników
na wyjściu i EWD, kategorii i ew. o trendzie (jeśli jest dla danego wskaźnika) oraz
informacje o wielkości miejscowości, rodzaju gminy i rodzaju szkoły?}

\item{tylkoWskDoPrezentacji}{wartość logiczna (domyślnie TRUE) - czy mają zostać
zwrócone wartości tylko tych wskaźników, które są zatwierdzone do prezentacji na
stronie?}

\item{tylkoWyswietlane}{wartość logiczna (domyślnie TRUE) - czy mają zostać
zwrócone wartości tylko dla tych szkół, dla których wyswietlane są elipsy? (ze
względu na kategorię, jaka została im przypisana)}

\item{tylkoNiePomin}{wartość logiczna (domyślnie TRUE) - czy pominąć wartości wskaźników
dla szkół specjalnych itp. (tj. takich, które nie były uwzględniane w grupie szkół,
na których estymowane były modele EWD i całkowicie pomijanych przy prezentacji
wskaźników na stronie)?}

\item{gamma}{poziom ufności (liczba z przedziału [0;1] )}

\item{fileEncoding}{ciąg znaków - strona kodowa, w której zostanie zapisany wynikowy
plik csv}

\item{maskaWskazniki}{opcjonalnie wyrażenie regularne, które powinny spełniać nazwy
wskażników (do wartości domyślnej - pustego ciągu znaków - pasują wszystkie nazwy)}

\item{src}{NULL połączenie z bazą danych IBE zwracane przez funkcję
\code{\link[ZPD]{polacz}}; pozwala posłużyć się niestandardowymi parametrami
połączenia}
}
\value{
data frame
}
\description{
Funkcja pobiera z bazy wartości wskaźników EWD. Domyślne wartości argumentów zostały
dobrane pod kątem wydawania wskazńików na zewnątrz.
}
\details{
Przykłady użycia - p. \href{http://zpd.ibe.edu.pl/doku.php?id=pobieranie_wartosci_ewd}{http://zpd.ibe.edu.pl/doku.php?id=pobieranie_wartosci_ewd}.
}
