#' @title Przekształcanie normalizacji ekwikwantylowej.
#' @description
#' Funkcja służy do sklejania ze sobą najniższych wartości skali znormalizowanej
#' ekwikwantylowo, przeliczając jednocześnie wartości skali.
#' @param wynikiZnorm wektor wartości na skali znormalizowanej
#' @param wynikiSurowe wektor wartości surowych, odpowiadających wartościom
#' znormalizowanym podanym w \code{wynikiZnorm}
#' @param do wartość liczbowa - wartość surowa, do jakiej należy kontynuować skracanie
#' (ale nie zostanie przeprowadzonych więcej złączeń, niż wartość argumentu \code{ile})
#' @param grupy opcjonalnie zmienna definiująca podział na grupy, w ramach których
#' oddzielnie ma być przeprowadzone łączenie
#' @param sr liczba - średnia znormalizowanej skali
#' @param os liczba - odchylenie standardowe znormalizowanej skali
#' @param ile wartość liczbowa - maksymalna liczba złączeń, które mogą zostać wykonane
#' @details
#' Tu w przyszłości uzupełnić. Trzeba dodać do funkcji jakąś weryfikację poprawności
#' argumentów.
#' @return Wektor liczbowy z przeliczonymi wartościami argumentu \code{wynikiZnorm}.
#' @seealso \code{\link[ZPD]{normalizuj_ekwikwantylowo}}
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
#' @param normy wektor opisujący unormowanie, typowo zwrócony przez funkcję
#' \code{\link[ZPD]{normalizuj_ekwikwantylowo}}
#' @param prefiksRok ciąg znaków postaci 'pr', gdzie p oznacza prefiks części egzaminu,
#' a r rok (zapis czterema cyframi)
#' @param zrodloDanychODBC opcjonalnie nazwa źródła danych ODBC, dającego dostęp do bazy
#' (domyślnie "EWD")
#' @details
#' Funkcja nie dopuszcza braków danych w argumencie \code{normy}.
#' Funkcja wymaga też, aby w ramach połączenia nawiązywanego z bazą mieć prawa do
#' modyfikacji tablic 'skale' i 'normy_ekwikwantylowe'.
#' @return funkcja nic nie zwraca
#' @seealso \code{\link[ZPD]{normalizuj_ekwikwantylowo}},
#' \code{\link{pobierz_wyniki_surowe_ewd_gimn}}
#' @export
zapisz_normy_do_bazy = function(normy, prefiksRok, zrodloDanychODBC="EWD") {
  stopifnot(is.numeric(normy), length(normy) > 0, all(!is.na(normy)),
            is.character(prefiksRok), length(prefiksRok) == 1,
            is.character(zrodloDanychODBC), length(zrodloDanychODBC) == 1)
  stopifnot(!is.null(names(normy)))
  stopifnot(all(!is.na(as.numeric(names(normy)))))

  stop("Funkcja wymaga przepisania")
#   wartosci = as.numeric(names(normy))
#   # sprawdzamy, czy odpowiednia skala już istnieje, a jeśli nie, to ją tworzymy
#   nazwa = paste("ewd", sub("^([^[:digit:]]+).*", "\\1", prefiksRok), sub(".*([[:digit:]]{4}$)", "\\1", prefiksRok), sep=";")
#   baza = odbcConnect(zrodloDanychODBC)
#   zapytanie = "SELECT * FROM skale WHERE nazwa = ?"
#   tryCatch({
#       skala = sqlExecute(baza, zapytanie, fetch=TRUE, stringsAsFactors=FALSE, data=nazwa)
#       odbcClose(baza)
#     },
#     error=function(e) {
#       odbcClose(baza)
#       stop(e)
#     }
#   )
#   if (nrow(skala) == 0) {
#     baza = odbcConnect(zrodloDanychODBC)
#     zapytanie = "INSERT INTO skale (id_skali, opis, nazwa) VALUES (nextval('skale_id_skali_seq'), '', ?)"
#     tryCatch({
#         skala = sqlExecute(baza, zapytanie, fetch=TRUE, stringsAsFactors=FALSE, data=nazwa)
#         odbcClose(baza)
#      },
#      error=function(e) {
#         odbcClose(baza)
#        stop(e)
#      }
#     )
#     baza = odbcConnect(zrodloDanychODBC)
#     zapytanie = "SELECT * FROM skale WHERE nazwa = ?"
#     tryCatch({
#         skala = sqlExecute(baza, zapytanie, fetch=TRUE, stringsAsFactors=FALSE, data=nazwa)
#         odbcClose(baza)
#       },
#       error=function(e) {
#         odbcClose(baza)
#         stop(e)
#       }
#     )
#   }
#   if (nrow(skala) > 1) stop("W bazie występuje więcej niż jedna skala o nazwie ", nazwa, ".")
#   # sprawdzamy, czy dla tej skali są już w bazie jakieś normy
#   baza = odbcConnect(zrodloDanychODBC)
#   zapytanie = "SELECT * FROM normy_ekwikwantylowe WHERE id_skali = ?"
#     tryCatch({
#       temp = sqlExecute(baza, zapytanie, fetch=TRUE, stringsAsFactors=FALSE, data=skala$id_skali)
#       odbcClose(baza)
#     },
#     error=function(e) {
#       odbcClose(baza)
#       stop(e)
#     }
#   )
#   if (nrow(temp) > 0) {
#     stop("W bazie znajdują się już normy przypisane do skali ", skala$id_skali, ". Najpierw je usuń.")
#   } else {
#     baza = odbcConnect(zrodloDanychODBC)
#     zapytanie = "INSERT INTO normy_ekwikwantylowe (id_skali, wartosc, wartosc_zr) VALUES (?, ?, ?)"
#     tryCatch({
#         temp = sqlExecute(baza, zapytanie, fetch=TRUE, stringsAsFactors=FALSE,
#                           data=data.frame(id_skali=skala$id_skali, wartosc=wartosci, wartosc_zr=normy))
#         odbcClose(baza)
#       },
#       error=function(e) {
#         odbcClose(baza)
#         stop(e)
#       }
#     )
#   }
#   invisible()
}
