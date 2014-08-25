#' @title Pobieranie danych o szkołach.
#' @description
#' Funkcja pobiera z bazy dane o szkołach - o ich typie i specyfice, nazwie, adresowe i o lokalizacji.
#' @param lata wektor liczb całkowitych - lata, których mają dotyczyć dane (dla każdej szkoły zwrócone zostaną tylko najświeższe dane w ramach tego okresu)
#' @param typySzkol opcjonalny wektor tekstowy z typami szkół, które mają zostać zwrócone (lub NULL - zwraca informacje o wszystkich szkołach)
#' @param idOke wartość logiczna (domyślnie FALSE) - czy dołączać kody OKE szkół?
#' @param daneAdresowe wartość logiczna (domyślnie FALSE) - czy dołączać nazwę i dane adresowe?
#' @param dolaczPaou wartość logiczna (domyślnie FALSE) - czy dołączać szkoły z danych PAOU?
#' @param zrodloDanychODBC opcjonalnie nazwa źródła danych ODBC, dającego dostęp do bazy (domyślnie "EWD")
#' @return data frame
#' @import RODBCext
#' @export
pobierz_dane_szkol <- function(lata, typySzkol=NULL, idOke=FALSE, daneAdresowe=FALSE, dolaczPaou=FALSE, zrodloDanychODBC="EWD"){
  stopifnot(is.numeric(lata)         , length(lata) > 0,
            is.character(typySzkol),
            is.logical(idOke)       , length(idOke) == 1,
            is.logical(daneAdresowe), length(daneAdresowe) == 1,
            is.logical(dolaczPaou)  , length(dolaczPaou) == 1,
            is.character(zrodloDanychODBC), length(zrodloDanychODBC) == 1
  )
  stopifnot(idOke %in% c(TRUE, FALSE), daneAdresowe %in% c(TRUE, FALSE), dolaczPaou %in% c(TRUE, FALSE))
  try(suppressWarnings(Sys.setlocale("LC_ALL", "pl_PL.UTF-8")))

  P = odbcConnect(zrodloDanychODBC)

  zapytanie = paste0( "SELECT id_szkoly, typ_szkoly, publiczna, dla_doroslych, specjalna, przyszpitalna,
                        d.rok, ",
                      ifelse(daneAdresowe, "d.nazwa, d.adres, d.miejscowosc, d.pna, ", ""),
                      ifelse(idOke       , "d.id_szkoly_oke, ", ""),
                      "d.wielkosc_miejscowosci, id_gminy + 100*id_powiatu + 10000*id_wojewodztwa AS teryt",
                      ifelse( is.null(typySzkol) | any(typySzkol %in% c("LO","LP","T")) ,", d.matura_miedzynarodowa",""),
                      " FROM szkoly AS sz JOIN szkoly_dane AS d USING (id_szkoly)
                      WHERE sz.id_szkoly > 0 AND d.rok IN (", paste0(rep("?", length(lata)), collapse=", "), ")
                        AND d.rok = (SELECT max(rok) FROM szkoly_dane WHERE id_szkoly = d.id_szkoly AND rok IN (", paste0(lata, collapse=", "), "))",
                      ifelse( is.null(typySzkol), "", paste0(" AND sz.typ_szkoly IN (", paste0(rep("?", length(typySzkol)), collapse=", "), ")") ),
                      ifelse( dolaczPaou, "", " AND d.paou = FALSE"),
                      " ORDER BY id_szkoly")
  dane = if( is.null(typySzkol)){
    as.list(lata)
  } else{
    data.frame(as.list(lata), as.list(typySzkol), stringsAsFactors=FALSE)
  }

  tryCatch({
      ret = sqlExecute(P, zapytanie, fetch=TRUE, stringsAsFactors=FALSE, data=dane)
      odbcClose(P)
    },
    error=function(e) {
      odbcClose(P)
      stop(e)
    }
  )

  ret$typ_szkoly[ret$typ_szkoly=="TRUE"] = "T"
  if (!("SP" %in% typySzkol)) ret = ret[, names(ret) != "id_szkoly_obut"]

  typyWWynikach = typySzkol %in% ret$typ_szkoly
  if (any(!typyWWynikach)) warning("Nie znaleziono żadnych szkół typu/ów: ", paste0(typySzkol[!typyWWynikach], collapse=", "), ".")

  attributes(ret)$lata = lata
  return( ret )
}
