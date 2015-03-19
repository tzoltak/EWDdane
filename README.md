# EWDdane

[![Travis-CI Build Status](https://travis-ci.org/tzoltak/EWDdane.png?branch=master)](https://travis-ci.org/tzoltak/EWDdane)
[![Coverage Status](https://coveralls.io/repos/tzoltak/EWDdane/badge.svg)](https://coveralls.io/r/tzoltak/EWDdane)

Pakiet zawiera funkcje służące do pobierania i zapisywania danych z bazy (o poziom wyżej niż to, co oferuje pakiet ZPD).

## Instalacja / aktualizacja

Pakiet nie jest wypchnięty na CRAN-a, więc instalować trzeba ze źródeł.

Ponieważ jednak zawiera jedynie kod w R, nie ma potrzeby zaopatrywać się w kompilatory, itp.

Instalacja możliwa jest w dwóch wariantach **(aby zaktualizować pakiet do najnowszej wersji należy zrobić dokładnie to samo)**:

1) Z użyciem pakietu devtools:
```r
install.packages('devtools') # potrzbne tylko, gdy nie jest jeszcze zainstalowany
devtools::install_github('tzoltak/EWDdane')
```

**Jeśli podczas instalacji napotkasz na błąd, a używasz linuksa** sprawdź, czy nie dotyczy Cię [ten problem](https://github.com/hadley/devtools/issues/650) lub przeprowadź "uczciwą instalację ze źródeł" (patrz niżej).

2) "Uczciwa instalacja ze źródeł":

   * Pobrać z sieci i zainstalować [narzędzia GIT-a dla linii komend](http://git-scm.com/downloads) 
   
   * W konsoli wywołać:
```r
git clone https://github.com/tzoltak/EWDdane.git
R CMD INSTALL ZPD
```
