#' @title Pobieranie danych do wyliczania wskaźników EWD gimnazjów.
#' @description
#' Funkcja pobiera dane surowe - w tym wyniki uzyskane za poszczególne kryteria oceny - egzaminu gimnazjalnego i połączone z nimi wyniki surowe sprawdzianu.
#' @param lataEG wektor liczb całkowitych - roczniki egzaminu gimnazjalnego, które mają zostać pobrane (wraz z przypisanymi wynikami sprawdzianu i innymi przydatnymi danymi)
#' @param dlKsztalcenia wektor liczb całkowitych - długość toku kształcenia, czyli z jakim przesunięciem wstecz powinny być pobierane wyniki sprawdzianu
#' @param usunSzkolySpecjalne wartość logiczna - czy usuwać uczniów szkół dla dorosłych, specjalnych, itp.?
#' @param jezykiObce wartość logiczna - czy zwrócone mają być również wyniki w zakresie języków obcych?
#' @param wyliczNormalizacje wartość logiczna - czy jeśli zostanie stwierdzone, że w bazie nie mazapisanych normalizacji ekwikwantylowych wyników egzaminów, to obliczyć je i zapisać do bazy?
#' @param zrodloDanychODBC opcjonalnie nazwa źródła danych ODBC, dającego dostęp do bazy (domyślnie "EWD")
#' @param backupZapisz nazwa pliku .RData, do którego zostaną zapisane dane po pobraniu i scaleniu, ale przed wyliczeniem sum i normalizacją
#' @param backupWczytaj nazwa pliku .RData, zapisanego przy wcześniejszym wywołaniu funkcji z wykorzystaniem argumentu \code{backupZapisz}
#' @details
#' Tu się przyda jakiś opis
#' @return Data frame, który zawiera: i tu też trzeba będzie opisać.
#' @import RODBCext
#' @import ZPD
#' @export
pobierz_wyniki_surowe_ewd_gimn = function(lataEG, dlKsztalcenia=3:4, usunSzkolySpecjalne=TRUE, jezykiObce=FALSE, wyliczNormalizacje=TRUE, zrodloDanychODBC="EWD", backupZapisz=NULL, backupWczytaj=NULL) {
  stopifnot(is.numeric(lataEG       ), length(lataEG) > 0,
            is.numeric(dlKsztalcenia), length(dlKsztalcenia) > 0,
            is.logical(usunSzkolySpecjalne), length(usunSzkolySpecjalne) == 1,
            is.logical(jezykiObce         ), length(jezykiObce) == 1,
            is.logical(wyliczNormalizacje ), length(wyliczNormalizacje) == 1,
            is.character(zrodloDanychODBC), length(zrodloDanychODBC) == 1
  )
  stopifnot(usunSzkolySpecjalne %in% c(TRUE, FALSE),
            jezykiObce %in% c(TRUE, FALSE), wyliczNormalizacje %in% c(TRUE, FALSE),
            lataEG %in% (2002:as.numeric(format(Sys.time(), "%Y"))),
            dlKsztalcenia %in% (3:5)
  )
  lataEG = unique(na.omit(lataEG))
  if (is.null(backupWczytaj)) {
    # pobieranie danych o szkołach
    message("Pobieranie danych o gimnazjach.")
    gimnazja = pobierz_dane_szkol(lataEG, "gimn.", zrodloDanychODBC=zrodloDanychODBC)
    if (usunSzkolySpecjalne) gimnazja = subset(gimnazja, with(gimnazja, {dla_doroslych == 0 & specjalna == 0 & przyszpitalna == 0})) # składnia z with trochę nadmiarowa, ale pozwala uniknąć NOTE przy sprawdzaniu pakietu
    names(gimnazja) = paste0(names(gimnazja), "_g")
    # i o wielokrotnie przystępujących
    message("Pobieranie danych o wielokrotnie przystępujących do egzaminów.")
    wielokrotnieEG  = wielokrotnie_przystepujacy("egzamin gimnazjalny", zrodloDanychODBC=zrodloDanychODBC)
    wielokrotnieSpr = wielokrotnie_przystepujacy("sprawdzian", zrodloDanychODBC=zrodloDanychODBC)

    # pobieranie egzaminu gimnazjalnego
    message("Pobieranie wyników egzaminu gimnazjalnego.")
    eG = kryteriaEG = list()
    prefiksy = c()
    for (i in lataEG) {
      baza = odbcConnect(zrodloDanychODBC)
      zapytanie = paste0("SELECT DISTINCT czesc_egzaminu, prefiks
                          FROM arkusze JOIN sl_czesci_egzaminow USING (rodzaj_egzaminu, czesc_egzaminu)
                          WHERE rodzaj_egzaminu='egzamin gimnazjalny'
                          AND EXTRACT (YEAR FROM data_egzaminu) = ?")
      tryCatch({
          czesciEG = sqlExecute(baza, zapytanie, fetch = TRUE, stringsAsFactors = FALSE, data=i)
          odbcClose(baza)
        },
        error=function(e) {
          odbcClose(baza)
         stop(e)
        }
      )
      prefiksy = c(prefiksy, czesciEG$prefiks)
      if (!jezykiObce) czesciEG = czesciEG[!grepl("^j[.] ", czesciEG$czesc_egzaminu) | czesciEG$czesc_egzaminu == "j. polski", ]

      for (j in czesciEG$czesc_egzaminu) {
        message("  Część ", j, " ", i, appendLF=FALSE)
        eG[[length(eG) + 1]] = try(pobierz_czesc_egzaminu("egzamin gimnazjalny", j, i, czyEwd=TRUE, zrodloDanychODBC=zrodloDanychODBC))
        eG[[length(eG)]] = cbind(rok_g = i, eG[[length(eG)]])
        nTemp = nrow(eG[[length(eG)]])
        message(": pobrano ", nTemp, " rekordów.")
        # usuwamy przystępujących nie po raz pierwszy
        eG[[length(eG)]] = eG[[length(eG)]][ !( eG[[length(eG)]]$id_obserwacji %in% with(wielokrotnieEG, {id_obserwacji[rok_min != i]}) ), ]
        message("    Usunięto ", nTemp - nrow(eG[[length(eG)]]), " rekord(y/ów) osób, które przystępowały do egzaminu po raz kolejny.")
        # drobne zmiany w nazwach zmiennych, po których nie powinno iść potem łączenie wyników różnych części egzaminu
        names(eG[[length(eG)]]) = sub("^(id_testu|opis_testu|laureat|zrodlo)$",
                                      paste0("\\1", "_", with(czesciEG, {prefiks[czesc_egzaminu == j]})),
                                      names(eG[[length(eG)]]))
        # zapisywanie, które kryteria związane z jaką częścią
        kryteriaEG[[length(eG)]] = names(eG[[length(eG)]])[ grep("^[kp]_[[:digit:]]+", names(eG[[length(eG)]] ))]
      }
      names(eG)[(length(eG) - nrow(czesciEG) + 1):length(eG)] = paste0(czesciEG$prefiks, i)
      names(kryteriaEG) = names(eG)
    }
    # dodawanie informacji o przypisaniu kryteriów do części pol.-hum. i mat.przyr.
    for (i in lataEG) {
      if (!any(grepl("^gh[[:digit:]]", names(kryteriaEG)[grep(i, names(kryteriaEG))]))) {
        kryteriaEG[[length(kryteriaEG) + 1]] = unlist(kryteriaEG[grep(paste0("^gh_[hp]", i), names(kryteriaEG))])
        names(kryteriaEG)[length(kryteriaEG)] = paste0("gh", i)
      }
      if (!any(grepl("^gm[[:digit:]]", names(kryteriaEG)[grep(i, names(kryteriaEG))]))) {
        kryteriaEG[[length(kryteriaEG) + 1]] = unlist(kryteriaEG[grep(paste0("^gm_[mp]", i), names(kryteriaEG))])
        names(kryteriaEG)[length(kryteriaEG)] = paste0("gm", i)
      }
    }
    # łączenie w jeden zbiór
    message("Łączenie wyników poszczególnych cześci egzaminu gimnazjalnego.")
    for (i in lataEG) {
      maskaRok = grep(paste0(i, "$"), names(eG))
      if (length(maskaRok) > 1) {
        eG[maskaRok[1]][[1]] = join_all(eG[maskaRok], type="full")
        eG[maskaRok[2:length(maskaRok)]] = NULL
        # przyłączanie informacji o uczniach
        maskaIdTestow = grep("^id_testu_", names( eG[maskaRok[1]][[1]] ))
        idTestow = lapply(eG[maskaRok[1]][[1]][, maskaIdTestow], unique)
        idTestow = na.omit(unique(unlist(idTestow)))
        eG[maskaRok[1]][[1]] = suppressMessages(join(
          pobierz_dane_uczniow(idTestow),
          eG[maskaRok[1]][[1]], type="right"
        ))
      }
    }
    eG = eG[!unlist(lapply(eG, is.null))]
    eG = rbind.fill(eG)
    # na wszelki wypadek warto sprawdzić...
    dupl = duplicated(eG$id_obserwacji)
    if (any(dupl)) warning("W połączonych wynikach egzaminu gimnazjalnego pojawiły się duplikaty id_obserwacji!!!")
    # zmiany nazw zmiennych, żeby nie kolidowały z tymi odnoszącymi się do egzaminu gimnazjalnego
    maskaZmienne = !grepl(paste0("^[kp]_[[:digit:]]+|^id_obserwacji$|^plec$|_(g|", paste0( unique(sub("[[:digit:]]{4}$", "", names(kryteriaEG))), collapse="|"), ")$"), names(eG))
    names(eG)[maskaZmienne] = paste0(names(eG)[maskaZmienne], "_g")

    # pobieranie sprawdzianu
    lataSpr = expand.grid(lataEG, dlKsztalcenia)
    lataSpr = sort(unique(lataSpr$Var1 - lataSpr$Var2), decreasing=TRUE)
    lataSpr = lataSpr[lataSpr >= 2002]
    spr = kryteriaSpr = list()
    for (i in lataSpr) {
      message("Sprawdzian ", i, appendLF=FALSE)
      spr[[length(spr) + 1]] = try(pobierz_czesc_egzaminu("sprawdzian", "", i, czyEwd=TRUE, zrodloDanychODBC=zrodloDanychODBC))
      spr[[length(spr)]] = cbind(rok_s = i, spr[[length(spr)]])
      nTemp = nrow(spr[[length(spr)]])
      message(": pobrano ", nTemp, " rekordów.")
      # usuwamy przystępujących nie po raz ostatni
      spr[[length(spr)]] = spr[[length(spr)]][ !( spr[[length(spr)]]$id_obserwacji %in% with(wielokrotnieSpr, {id_obserwacji[rok_max != i]}) ), ]
      message("    Usunięto ", nTemp - nrow(spr[[length(spr)]]), " rekord(y/ów) osób, które przystępowały do egzaminu nie po raz ostatni.")
      # zapisywanie, które kryteria związane z jaką częścią
      kryteriaSpr[[length(spr)]] = names(spr[[length(spr)]])[ grep("^[kp]_[[:digit:]]+", names(spr[[length(spr)]] ))]
      names(kryteriaSpr)[length(spr)] = paste0("s", i)
      # przyłączanie informacji o uczniach
      spr[[length(spr)]] = suppressMessages(join(
        pobierz_dane_uczniow(unique(spr[[length(spr)]]$id_testu)),
        spr[[length(spr)]], type="right"
      ))
    }
    # łączenie w jeden zbiór
    message("Łączenie wyników sprawdzianu z poszczególnych lat.")
    spr = rbind.fill(spr)
    # zmiany nazw zmiennych, żeby nie kolidowały z tymi odnoszącymi się do egzaminu gimnazjalnego
    maskaZmienne = !grepl("^[kp]_[[:digit:]]+|^id_obserwacji$|^plec$|_s$", names(spr))
    names(spr)[maskaZmienne] = paste0(names(spr)[maskaZmienne], "_s")
    # na wszelki wypadek warto sprawdzić...
    dupl = duplicated(spr$id_obserwacji)
    if (any(dupl)) warning("W połączonych wynikach sprawdzianu pojawiły się duplikaty id_obserwacji!!!")

    # łączenie
    message("Przyłączanie wyników sprawdzianu do wyników egzaminu gimnazjalnego.")
    eG = suppressMessages(join(eG, spr, type="left"))
    rm(spr)
    message("Przyłączanie informacji o gimnazjach.")
    nTemp = nrow(eG)
    eG = suppressMessages(join(gimnazja, eG, type="left"))
    message("  Usunięto ", nTemp - nrow(eG), " rekord(y/ów) uczniów szkół dla dorosłych, specjalnych, itp.")

    kryteria = unlist(list(kryteriaEG, kryteriaSpr), recursive=FALSE)
    if (!is.null(backupZapisz)) save(eG, kryteria, file=backupZapisz)
  } else {
    load(backupWczytaj)
  }
  # wyliczanie sum
  message("Wyliczanie łącznych wyników surowych.")
  for (i in 1:length(kryteria)) {
    sumaTemp = apply(eG[, kryteria[[i]] ], 1,
                     function(x) {
                       if (all(is.na(x))) {
                         return(NA)
                       } else {
                         return(sum(x, na.rm=TRUE))
                       }
                     })
    eG = cbind(eG, sumaTemp)
    names(eG)[ncol(eG)] = paste0("sum_", names(kryteria)[i])
    # wstawianie braków danych dla sum kilku testów, gdy ktoś nie napisał jednego z testów składowych (ale napisał jakiś inny, więc w tej chwili ma przypisany wynik)
    if (!is.null(names( kryteria[[i]] ))) {  # taki dziwny warunek, ale działa :)
      prefiksyTestow = unique(gsub("^([^[:digit:]]+).*$", "\\1", names( kryteria[[i]] )))
      rok = gsub(".*([[:digit:]]{4})$", "\\1", names(kryteria)[i] )
      brakiDanych = apply(eG[, paste0("sum_", prefiksyTestow, rok)], 1, function(x) {return(any(is.na(x)))})
      eG[brakiDanych, paste0("sum_", names(kryteria)[i])] = NA
    }
  }

  # obsługa normalizacji
  if (wyliczNormalizacje) {
    message("Normalizacja ekwikwantylowa wyników.")
    normalizacje=list()
    baza = odbcConnect(zrodloDanychODBC)
    zapytanie = "SELECT nazwa, wartosc, wartosc_zr
                FROM normy_ekwikwantylowe JOIN skale USING (id_skali)
                WHERE nazwa LIKE 'ewd;g%' OR nazwa LIKE 'ewd;s%'
                ORDER BY nazwa, wartosc"
    tryCatch({
        normy = sqlExecute(baza, zapytanie, fetch = TRUE, stringsAsFactors = FALSE)
        odbcClose(baza)
      },
      error=function(e) {
        odbcClose(baza)
        stop(e)
      }
    )
    normy$nazwa = gsub("^ewd|;", "", normy$nazwa)
    normy = normy[normy$nazwa %in% names(kryteria), ]
    normyDla = unique(normy$nazwa)
    for (i in names(kryteria)) {
      message("  ", i)
      nazwaSuma = paste0("sum_", i)
      rok = as.numeric(gsub(".*([[:digit:]]{4})$", "\\1", i))
      maskaRok = eG[, paste0("rok_", substr(i, 1, 1))] == rok
      if (i %in% normyDla) {
        normyTemp = normy[normy$nazwa == i, ]
      } else {  # bez norm w bazie
        if (substr(i, 1, 1) == "g") {  # egzamin gimnazjalny
          normyTemp = normy_ekwikwantylowe(eG[, nazwaSuma])
        } else {  # sprawdzian
          message("    Pobieranie wyników egzaminu (dane CKE) z bazy.")
          spr = pobierz_czesc_egzaminu("sprawdzian", "", rok, zrodloDanychODBC=zrodloDanychODBC, czyEwd=FALSE)
          suma = apply(spr[, grep("^[kp]_", names(spr))], 1,
                       function(x) {
                         if (all(is.na(x))) {
                           return(NA)
                         } else {
                           return(sum(x, na.rm=TRUE))
                         }
                       })
          normyTemp = normy_ekwikwantylowe(suma)
        }
        tryCatch({nic = zapisz_normy_do_bazy(normyTemp, i, zrodloDanychODBC=zrodloDanychODBC)},
                 error=function(e) {warning(e, immediate.=TRUE)})
         normyTemp = data.frame(nazwa=i, wartosc=as.numeric(names(normyTemp)), wartosc_zr=normyTemp)
      }
      normalizacje[[length(normalizacje) + 1]] = normyTemp
      normyTemp = normyTemp$wartosc_zr[eG[, nazwaSuma] + 1]
      normyTemp[!maskaRok] = NA
      eG = cbind(eG, normyTemp)
      names(eG)[ncol(eG)] = paste0("norm_", i)
    }
    names(normalizacje) = names(kryteria)
  }

  # łączenie sum (i ew. normalizacji) z różnych lat w jedne zmienne
  for (i in unique( sub("[[:digit:]]{4}$", "", names(kryteria)) )) {
    maskaZmienne = grep(paste0("^sum_", i, "[[:digit:]]{4}$"), names(eG))
    names(eG)[maskaZmienne][1] = sub("[[:digit:]]{4}$", "", names(eG)[maskaZmienne][1])
    if (length(maskaZmienne) > 1) {
      eG[, maskaZmienne[1]] = apply(eG[, maskaZmienne], 1, function(x) {return(x[!is.na(x)][1])})
      eG = eG[, -maskaZmienne[-1]]
    }
    if (wyliczNormalizacje) {
      maskaZmienne = grep(paste0("^norm_", i, "[[:digit:]]{4}$"), names(eG))
      names(eG)[maskaZmienne][1] = sub("[[:digit:]]{4}$", "", names(eG)[maskaZmienne][1])
      if (length(maskaZmienne) > 1) {
        eG[, maskaZmienne[1]] = apply(eG[, maskaZmienne], 1, function(x) {return(x[!is.na(x)][1])})
        eG = eG[, -maskaZmienne[-1]]
      }
    }
  }

  # porządki z formatami zmiennych i koniec!
  message("Porządki z formatami zmiennych.")
  if (!("laureat_gh") %in% names(eG)) eG = cbind(eG, laureat_gh = with(eG, as.numeric(laureat_gh_p == 1 | laureat_gh_h == 1)))
  if (!("laureat_gm") %in% names(eG)) eG = cbind(eG, laureat_gm = with(eG, as.numeric(laureat_gm_m == 1 | laureat_gm_p == 1)))
  eG = formaty_zmiennych_baza_na_ewd(eG)
  attributes(eG)$kryteria = kryteria
  attributes(eG)$normy = normalizacje
  return(eG)
}
