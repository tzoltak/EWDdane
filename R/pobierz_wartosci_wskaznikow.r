#' @title Pobieranie wartosci wskaznikow EWD.
#' @description
#' Funkcja pobiera z bazy wartości wskaźników EWD. Domyślne wartości argumentów zostały
#' dobrane pod kątem wydawania wskazńików na zewnątrz.
#' @param typSzkoly ciąg znaków: 'gimn.', 'LO' lub 'T'
#' @param lata wektor liczb całkowitych - lata, dla których mają zostać pobrane wskaźniki
#' (wskaźniki identyfikowane są po ostatnim roku w ramach okresu, np. wskaźniki 2013-2011
#' uzyskamy podając jako argument \code{lata} wartość 2013)
#' @param zapis opcjonalnie nazwa pliku, do którego zostaną zapisane pobrane dane
#' (w formacie csv)
#' @param jst opcjonalnie wyrażenie regularne opisujące TERYTy JST, dla których mają
#' zostać zwrócone wskaźniki lub NULL, jeśli mają zostać zwrócone wartości wskaźników dla
#' całego kraju
#' @param idOke wartość logiczna (domyślnie FALSE) - czy dołączać kody OKE szkół?
#' @param daneEWD wartość logiczna (domyślnie FALSE) - czy dołączać informacje o wielkości
#' miejscowości i o tym, czy szkoła jest publiczna?
#' @param daneAdresowe wartość logiczna (domyślnie TRUE) - czy dołączać dane adresowe
#' szkół?
#' @param ladneNazwy wartość logiczna (domyślnie TRUE) - czy zmieniać nazwy kolumn na
#' pięknie opisowe?
#' @param fileEncoding ciąg znaków - strona kodowa, w której zostanie zapisany wynikowy
#' plik csv
#' @param zrodloDanychODBC opcjonalnie nazwa źródła danych ODBC, dającego dostęp do bazy
#' (domyślnie "EWD")
#' @return data frame
#' @import RODBCext
#' @export
pobierz_wartosci_wskaznikow = function(typSzkoly, lata, zapis=NULL, jst=NULL, idOke=FALSE, daneEWD=FALSE, daneAdresowe=TRUE, ladneNazwy=TRUE, fileEncoding="windows-1250", zrodloDanychODBC="EWD"){
  stopifnot(is.numeric(lata)        , length(lata) > 0,
            is.character(typSzkoly) , length(typSzkoly) == 1,
            is.null(jst) | is.character(jst), is.null(jst) | length(jst) == 1,
            is.null(zapis) | is.character(zapis),
            is.null(zapis) | length(zapis) == 1,
            is.logical(idOke)       , length(idOke) == 1,
            is.logical(daneEWD)     , length(daneEWD) == 1,
            is.logical(daneAdresowe), length(daneAdresowe) == 1,
            is.logical(ladneNazwy)  , length(ladneNazwy) == 1,
            is.character(fileEncoding), length(fileEncoding) == 1,
            is.character(zrodloDanychODBC), length(zrodloDanychODBC) == 1
  )

  # przygotowanie struktury danych
  tryCatch(
    {
      baza = odbcConnect(zrodloDanychODBC)
      nazwyWskaznikow = sqlExecute(baza, paste0("
        SELECT DISTINCT id_wskaznika, tablica_danych, skrot, kolejnosc
        FROM sl_wskazniki JOIN sl_wskazniki_typy_szkol USING (id_wskaznika)
        WHERE typ_szkoly='", typSzkoly, "'
          AND paou IS FALSE AND do_prezentacji IS TRUE
        ORDER BY kolejnosc"),
        fetch = TRUE, stringsAsFactors = FALSE)
    },
    error = stop,
    finally = odbcClose(baza)
  )
  tablicaWsk = unique(nazwyWskaznikow$tablica_danych)
  wskazniki = vector(mode="list", length=nrow(nazwyWskaznikow))
  names(wskazniki) = nazwyWskaznikow$id_wskaznika
  # pobieranie wskaźników
  for(i in names(wskazniki)) {
    tryCatch(
      {
        baza = odbcConnect(zrodloDanychODBC)
        wskazniki[[i]] = sqlExecute(baza, paste0("
          SELECT id_szkoly, rok_do,
            sr_wynik_egz - 2.447747 * bs_sr_wynik_egz AS dg_pu_sr_wynik_egz,
            sr_wynik_egz + 2.447747 * bs_sr_wynik_egz AS gg_pu_sr_wynik_egz,
            sr_wynik_egz,
    	      ewd - 2.447747 * bs_ewd AS dg_pu_ewd,
            ewd + 2.447747 * bs_ewd AS gg_pu_ewd,
            ewd,
            lu_ewd AS lu
	        FROM ", tablicaWsk, "
	        WHERE
		        kategoria IN (SELECT kategoria FROM sl_kategorie WHERE wyswietlaj IS TRUE)
		        AND id_wskaznika='", i, "'
		        AND rok_do IN (", paste0(lata, collapse=", "), ")"),
          fetch=TRUE)
      },
      error = stop,
      finally = odbcClose(baza)
    )
  }
  wskazniki = wskazniki[unlist(lapply(wskazniki, nrow)) > 0]
	wskazniki = mapply(
		function(x, nazwa) {
      maska = !(names(x))%in%c("id_szkoly", "rok_do")
			names(x)[maska] = paste0(names(x)[maska], "_", nazwa)
			return(x)
		},
		wskazniki, as.list(names(wskazniki)),
		SIMPLIFY = FALSE
	)
	wskazniki = suppressMessages(join_all(wskazniki, type="full"))
  # połączenie z danymi szkół
  wskazniki = suppressMessages(join(
    pobierz_dane_szkol(c(lata, min(lata) - (1:2)), typSzkoly,
                       idOke = idOke, daneAdresowe = daneAdresowe,
                       dolaczPaou = FALSE, zrodloDanychODBC = zrodloDanychODBC),
    wskazniki,
    type="inner"
  ))
  # usuwanie niepotrzebnych kolumn
  wskazniki = wskazniki[, !(names(wskazniki) %in% c("dla_doroslych", "specjalna",
                                                    "przyszpitalna", "rok"))]
  if (!daneEWD) {
    wskazniki = wskazniki[, !(names(wskazniki) %in% c("wielkosc_miejscowosci", "publiczna"))]
  }
  # filtrowanie JST
  if(!is.null(jst)){
    wskazniki = wskazniki[grepl(jst, wskazniki$teryt), ]
  }
	# porządki
	maskaNazwyId = grepl("^id_|^(rok_do|teryt)$", names(wskazniki))
	wskazniki = wskazniki[, c(names(wskazniki)[ maskaNazwyId],
                            names(wskazniki)[!maskaNazwyId])]
  # ew. piękne nazwy kolumn
  if (ladneNazwy) {
    maska = grep("ewd_|sr_wynik_egz_", names(wskazniki))
    wskazniki[maska] = lapply(wskazniki[maska], round, digits=2)
    names(wskazniki) = sub("id_szkoly", "id szkoły w bazie EWD", names(wskazniki))
    names(wskazniki) = sub("teryt", "TERYT gminy", names(wskazniki))
    names(wskazniki) = sub("rok_do", "ostatni rok okresu obejmowanego przez wskaźnik",
                           names(wskazniki))
    names(wskazniki) = sub("typ_szkoly", "typ szkoły", names(wskazniki))
    names(wskazniki) = sub("miejscowosc", "miejscowość", names(wskazniki))
    names(wskazniki) = sub("dg_pu_", "dolna granica przedz. ufności dla ", names(wskazniki))
    names(wskazniki) = sub("gg_pu_", "górna granica przedz. ufności dla ", names(wskazniki))
    names(wskazniki) = sub("sr_wynik_egz_", "śr. wyników egzaminów ", names(wskazniki))
    names(wskazniki) = sub("ewd_", "EWD ", names(wskazniki))
    names(wskazniki) = sub("^lu_", "liczba uczniów ", names(wskazniki))
    for (i in 1:nrow(nazwyWskaznikow)) {
      names(wskazniki) = sub(nazwyWskaznikow$id_wskaznika[i],
                             paste0(" - wsk. ", nazwyWskaznikow$skrot[i]),
                             names(wskazniki))
    }
  }
	# zapis
	if (!is.null(zapis)) {
    write.csv2(wskazniki, zapis, row.names=FALSE, na="", fileEncoding=fileEncoding)
    invisible(wskazniki)
	} else {
    return(wskazniki)
	}
}
