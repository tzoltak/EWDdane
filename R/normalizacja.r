#' @title Normalizacja ekwikwantylowa.
#' @description
#' Funkcja wylicza normalizację ekwikwantylową zadanej zmiennej.
#' @param x wektor nieujemnych liczb całkowitych, na podstawie którego ma zostać wyliczona normalizacja
#' @param max liczba, opcjonalnie maksymalna wartość skali, na jakiej wyrażony jest x
#' @param prog liczba, opcjonalnie, wszystkim wartościom, których skumulowana częstość występowania jest mniejsza niż \code{prog} lub większa niż \code{1-prog} zostaną przypisane wartości znormalizowane odpowiadające skumulowanej częstości odpowiednio \code{prog} i \code{1-prog}
#' @param sr liczba - średnia znormalizowanej skali
#' @param os liczba - odchylenie standardowe znormalizowanej skali
#' @param uzupLuki wartość logiczna - czy jeśli pomiędzy wartościami występującymi w wektorze \code{x} (oraz 0 i ew. wartością parametru \code{max}, jeśli został podany) istnieją liczby całkowite, które nie wystąpiły w \code{x}, to wartości znormalizowane mają być wyliczone również dla nich?
#' @param ekstrapolacja wartość logiczna - czy w przypadku, gdy wartości znormalizowane mają być przypisane wartościom niższym, niż najmniejsza występująca w wektorze \code{x} oraz większym, niż największa występujacaw wektorze \code{x}, to mają być one wyliczone poprzez ekstrapolację liniową?
#' @details
#' Normalizacja wyliczana jest poprzez przekształcenie postaci: \code{u(X=i) = sr + os * F( [N(X<i) + N(X=i)/2 ] / n )}, gdzie \code{n} to liczba obserwacji (liczba elementów wektora \code{x} nie będących brakami danych), a \code{F} to funkcja odrotna do dystrybuanty rozkładu normalnego stanaryzowanego.
#' Jeśli parametr \code{uzupLuki} ma wartość \code{FALSE}, to wartości, które nie wystąpiły w wektorze \code{x} nie zostaną uwzględnione w wynikach. Jeśli ma wartość \code{TRUE}, to wartości znormalizowane zostaną im przypisane na podstawie interpolacji/ekstrapolacji liniowej w oparciu o dwie najbliższe wartości (większą i mniejszą), które wystąpiły w wektorze \code{x}. Wartościom mniejszym niż najmniejsze i większym niż największe wartości, które wystąpiły w wektorze \code{x} przypisane zostaną wartości znormalizowane odpowiadające najmniejszej/największej wartości, która wystąpiła w danych (jeśli parametr \code{ekstrapolacja} przyjmuje wartość \code{FALSE}). Jeśli parametr \code{ekstrapolacja} przyjmuje wartość \code{TRUE}, wartościom tym zostaną przypisane wartości znormalizowane na podstawie ekstrapolacji liniowej w oparciu o dwie najmniejsz/największe wartości, które wystąpiły w wektorze \code{x}.
#' @return Wektor liczbowy wartości znormalizowanych, z przypisanymi nazwami opisującymi wartości wejściowej zmiennej.
#' @seealso \code{\link{sklej_normy}}
#' @export
normy_ekwikwantylowe = function(x, max=NULL, prog=0.000001, sr=100, os=15, uzupLuki=TRUE, ekstrapolacja=FALSE) {
  stopifnot(is.numeric(x), length(x) > 1,
            is.numeric(x) | is.null(x),
            is.numeric(prog), length(prog) == 1,
            is.numeric(sr), length(sr) == 1,
            is.numeric(os), length(os) == 1,
            is.logical(uzupLuki), length(uzupLuki) == 1,
            is.logical(ekstrapolacja), length(ekstrapolacja) == 1)
  stopifnot(uzupLuki %in% c(TRUE, FALSE), ekstrapolacja %in% c(TRUE, FALSE))
  if (!is.null(max)) stopifnot(as.integer(max) == max)
  stopifnot(all(as.integer(x) == x | is.na(x)), all(x >=0 | is.na(x)),
            prog > 0, prog < 0.5,
            is.finite(sr), is.finite(os))
  if (any(!is.finite(x) & !is.na(x))) {
    x[!is.finite(x)] = NA
    warning("W danych wystąpiły wartości Inf, -Inf lub NaN, które zostały potraktowane jako braki danych.")
  }
  if (all(is.na(x))) {
    warning("Nie podano żadnych wartości nie będących brakami danych. Normalizacja nie może zostać przeprowadzona.")
    return(NA)
  }
  # zapewniam sobie uwzględnienie w rozkładzie również tych wartości punktowych, które nie występują w danych
  if (uzupLuki) {
    if (is.null(max)) {
      x = factor(x, levels=c(0:max(x, na.rm=TRUE)))
    } else {
      x = factor(x, levels=c(0:max))
    }
  }
  rozklad = table(x)
  skum = (cumsum(rozklad) - rozklad / 2) / sum(rozklad)
  skum[rozklad == 0] = NA
  skum[skum > (1 - prog)] = 1 - prog
  skum[skum < prog] = prog
  norma = qnorm(skum, 0, 1)

  # generowanie wartości znormalizowanych dla wartości surowych, które nie występują w danych
  zera = c(1:length(norma))[is.na(norma)]
  lz = length(zera)
  if (uzupLuki & lz > 0) {
    message("W danych nie wystąpiły wartości: ", paste0(names(norma)[zera], collapse=", "), ". Wartości znormalizowane dla nich zostały sztucznie wygenerowane.")
    zeraPocz = sum(zera == (1:lz))  # stwierdzamy ile dziur jest na początku skali
    zeraKon  = sum(zera == ((length(norma) - lz + 1):length(norma)))  # i na końcu
    if (zeraPocz > 0) zera = zera[-(1:zeraPocz)]  # i chwilowo bierzemy je w nawias
    if (zeraKon  > 0) zera = zera[-((lz-zeraKon+1):lz)+zeraPocz]  # i chwilowo bierzemy je w nawias
    i = 1
    lz = length(zera)
    while (i <= lz) {  # żeby najpierw zabrać się za ew. dziury w środku skali
      dlLuki = sum( zera[i:lz] == (zera[i]:( zera[i] + lz - i )) )
      norma[(zera[i] - 1):(zera[i] + dlLuki)] = seq(norma[zera[i] - 1],
                                                    norma[zera[i] + dlLuki],
                                                    (norma[zera[i] + dlLuki] - norma[zera[i] - 1]) / (dlLuki + 1))
      i = i + dlLuki
    }
    if (zeraPocz > 0) {
      if (ekstrapolacja) {
        # luki na początku wypełniamy w oparciu o różnicę między dwoma pierwszymi elementami, dla których mamy już wartości wyskalowane (tak to działało do 2013 r. włącznie)
        norma[1:zeraPocz] =
          norma[zeraPocz + 1] - c(zeraPocz:1) * (norma[zeraPocz +2] - norma[zeraPocz + 1])
      } else {
        # albo przypisujemy najniższą zanotowaną wartość (domyślne rozwiązanie od 2014 r.)
        norma[1:zeraPocz] = norma[zeraPocz + 1]
      }
    }
    ln = length(norma)
    if (zeraKon > 0) {
      if (ekstrapolacja) {
        # luki na końcu analogicznie, z tym że w oparciu o różnicę pomiędzy dwoma ostatnimi elementami, dla których mamy już wartości wyskalowane (tak to działało do 2013 r. włącznie)
        norma[-c(1:(ln - zeraKon))] = norma[ln - zeraKon] + c(1:zeraKon) * (norma[ln - zeraKon] - norma[ln - zeraKon - 1])
      } else {
        # albo przypisujemy najniższą zanotowaną wartość (domyślne rozwiązanie od 2014 r.)
        norma[-c(1:(ln - zeraKon))] = norma[ln - zeraKon]
      }
    }
  }

  # raczej się nie zdarza, żeby w wyniku działań jw. otrzymać przeliczenie, które da nam zmienną o średniej dokładnie 0 i odchyleniu standardowym dokładnie 1, więc trochę to jeszcze ręcznie dostroimy:
  rozklad = rozklad / sum(rozklad)
  przesuniecie = sum(norma * rozklad)
  skala = sum(rozklad * (norma - przesuniecie)^2 )^0.5
  norma = os * (norma - przesuniecie) / skala + sr

  return(setNames(as.numeric(norma), names(norma)))
}
#' @title Przekształcanie normalizacji ekwikwantylowej.
#' @description
#' Funkcja służy do sklejania ze sobą najniższych wartości skali znormalizowanej ekwikwantylowo, przeliczając jednocześnie wartości skali.
#' @param wynikiZnorm wektor wartości na skali znormalizowanej
#' @param wynikiSurowe wektor wartości surowych, odpowiadających wartościom znormalizowanym podanym w \code{wynikiZnorm}
#' @param do wartość liczbowa - wartość surowa, do jakiej należy kontynuować skracanie (ale nie zostanie przeprowadzonych więcej złączeń, niż wartość argumentu \code{ile})
#' @param grupy opcjonalnie zmienna definiująca podział na grupy, w ramach których oddzielnie ma być przeprowadzone łączenie
#' @param sr liczba - średnia znormalizowanej skali
#' @param os liczba - odchylenie standardowe znormalizowanej skali
#' @param ile wartość liczbowa - maksymalna liczba złączeń, które mogą zostać wykonane
#' @details
#' Tu w przyszłości uzupełnić. Trzeba dodać do funkcji jakąś weryfikację poprawności argumentów.
#' @return Wektor liczbowy z przeliczonymi wartościami argumentu \code{wynikiZnorm}.
#' @seealso \code{\link{normy_ekwikwantylowe}}
#' @export
sklej_normy = function(wynikiZnorm, wynikiSurowe, do, grupy=NULL, sr=100, os=15, ile=do) {
  if (is.null(grupy)) grupy = rep(1, length(wynikiZnorm))
  grupy = as.character(grupy)

  normyMin = by(wynikiZnorm, grupy, function(x) {return(sort(unique(x))[1:2])} )
  normyMinNowe = lapply(normyMin,
                      function(x, popr) {
                        return(qnorm(pnorm(x[2], sr, os) - pnorm(x[1], sr, os), sr, os))
                      }
  )
  for (i in 1:length(normyMin)) {
    if (wynikiSurowe[grupy == names(normyMin)[i] & wynikiZnorm == normyMin[[i]][2] & !is.na(wynikiZnorm)][1] <= do) {
      # jeśli w danych jakiś wynik spr. nie wystąpił, to nie sklejajmy ze sobą bez potrzeby wyższych wyników
      wynikiZnorm[grupy == names(normyMin)[i] & wynikiZnorm %in% normyMin[[i]] & !is.na(wynikiZnorm)] = normyMinNowe[[i]]
    }
  }

  if (ile > 1) wynikiZnorm = sklej_normy(wynikiZnorm, wynikiSurowe, do, grupy, sr, os, ile - 1)
  return(wynikiZnorm)
}
#' @title Zapis normalizacji ekwikwantylowej do bazy.
#' @description
#' Funkcja służy do zapisania do bazy wyliczonych norm ekwikwantylowych.
#' @param normy wektor opisujący unormowanie, typowo zwrócony przez funkcję \code{\link{normy_ekwikwantylowe}}
#' @param prefiksRok ciąg znaków postaci 'pr', gdzie p oznacza prefiks części egzaminu, a r rok (zapis czterema cyframi)
#' @param zrodloDanychODBC opcjonalnie nazwa źródła danych ODBC, dającego dostęp do bazy (domyślnie "EWD")
#' @details
#' Funkcja nie dopuszcza braków danych w argumencie \code{normy}.
#' Funkcja wymaga też, aby w ramach połączenia nawiązywanego z bazą mieć prawa do modyfikacji tablic 'skale' i 'normy_ekwikwantylowe'.
#' @return funkcja nic nie zwraca
#' @seealso \code{\link{normy_ekwikwantylowe}}, \code{\link{pobierz_wyniki_surowe_ewd_gimn}}
#' @import RODBCext
#' @export
zapisz_normy_do_bazy = function(normy, prefiksRok, zrodloDanychODBC="EWD") {
  stopifnot(is.numeric(normy), length(normy) > 0, all(!is.na(normy)),
            is.character(prefiksRok), length(prefiksRok) == 1,
            is.character(zrodloDanychODBC), length(zrodloDanychODBC) == 1)
  stopifnot(!is.null(names(normy)))
  stopifnot(all(!is.na(as.numeric(names(normy)))))

  wartosci = as.numeric(names(normy))
  # sprawdzamy, czy odpowiednia skala już istnieje, a jeśli nie, to ją tworzymy
  nazwa = paste("ewd", sub("^([^[:digit:]]+).*", "\\1", prefiksRok), sub(".*([[:digit:]]{4}$)", "\\1", prefiksRok), sep=";")
  baza = odbcConnect(zrodloDanychODBC)
  zapytanie = "SELECT * FROM skale WHERE nazwa = ?"
  tryCatch({
      skala = sqlExecute(baza, zapytanie, fetch=TRUE, stringsAsFactors=FALSE, data=nazwa)
      odbcClose(baza)
    },
    error=function(e) {
      odbcClose(baza)
      stop(e)
    }
  )
  if (nrow(skala) == 0) {
    baza = odbcConnect(zrodloDanychODBC)
    zapytanie = "INSERT INTO skale (id_skali, opis, nazwa) VALUES (nextval('skale_id_skali_seq'), '', ?)"
    tryCatch({
        skala = sqlExecute(baza, zapytanie, fetch=TRUE, stringsAsFactors=FALSE, data=nazwa)
        odbcClose(baza)
     },
     error=function(e) {
        odbcClose(baza)
       stop(e)
     }
    )
    baza = odbcConnect(zrodloDanychODBC)
    zapytanie = "SELECT * FROM skale WHERE nazwa = ?"
    tryCatch({
        skala = sqlExecute(baza, zapytanie, fetch=TRUE, stringsAsFactors=FALSE, data=nazwa)
        odbcClose(baza)
      },
      error=function(e) {
        odbcClose(baza)
        stop(e)
      }
    )
  }
  if (nrow(skala) > 1) stop("W bazie występuje więcej niż jedna skala o nazwie ", nazwa, ".")
  # sprawdzamy, czy dla tej skali są już w bazie jakieś normy
  baza = odbcConnect(zrodloDanychODBC)
  zapytanie = "SELECT * FROM normy_ekwikwantylowe WHERE id_skali = ?"
    tryCatch({
      temp = sqlExecute(baza, zapytanie, fetch=TRUE, stringsAsFactors=FALSE, data=skala$id_skali)
      odbcClose(baza)
    },
    error=function(e) {
      odbcClose(baza)
      stop(e)
    }
  )
  if (nrow(temp) > 0) {
    stop("W bazie znajdują się już normy przypisane do skali ", skala$id_skali, ". Najpierw je usuń.")
  } else {
    baza = odbcConnect(zrodloDanychODBC)
    zapytanie = "INSERT INTO normy_ekwikwantylowe (id_skali, wartosc, wartosc_zr) VALUES (?, ?, ?)"
    tryCatch({
        temp = sqlExecute(baza, zapytanie, fetch=TRUE, stringsAsFactors=FALSE,
                          data=data.frame(id_skali=skala$id_skali, wartosc=wartosci, wartosc_zr=normy))
        odbcClose(baza)
      },
      error=function(e) {
        odbcClose(baza)
        stop(e)
      }
    )
  }
  invisible()
}
