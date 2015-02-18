#' @title Zapisywanie parametrow skalowan.
#' @description
#' Funkcja zapisuje parametry skalowania do bazy danych
#' @param nazwa_skali nazwa skali. Kiedy nazwa skali przymuje wartość NULL nie jest
#' brana pod
#' uwagę w wyszukiwaniu. Wartość domyślna to NULL.
#' @param id_testu id testu. Kiedy id testu przymuje wartość NULL nie jest brana pod
#' uwagę w wyszukiwaniu. Wartość domyślna to NULL.
#' @param parametry ramka danych o strukturze zgodnej z tą, w jakiej zwraca oszacowania
#' parametrów funkcja skaluj().
#' @param opis opis estymacji
#' @param estymacja parametr opisujący metodę estymacji - musi być jedną z wartości
#' występujących w tablicy sl_estymacje_parametrow.
#' @param rEAP rzetelność empiryczna, która domyśnie wynosi NULL.
#' @param zrodloDanychODBC string określający źródło danych. Wartość domyślna to 'EWD'.
#' @return Funkcja zwraca id skalowania.
#' @export
zapisz_parametry_skalowania = function(nazwa_skali=NULL, id_testu=NULL, parametry, opis,
                                       estymacja, rEAP = NULL,  zrodloDanychODBC="EWD") {
  if( is.null(nazwa_skali) & is.null(id_testu) ){
    stop("Nazwa skali oraz id testu nie mogą mieć jednocześnie wartości null.")
  }
  if( !is.null(id_testu)  & !is.numeric(id_testu) ){
    stop("Id testu nie jest liczbą.")
  }
  if(  !is.null(nazwa_skali)  & !is.character(nazwa_skali) ){
    stop("Nazwa skali nie jest ciągiem znaków.")
  }
  if( !is.character(opis) ){
    stop("Opis nie jest ciągiem znaków.")
  }
  if( !is.character(estymacja) ){
    stop("Estymacja nie jest ciągiem znaków.")
  }
  if( !is.character(zrodloDanychODBC) ){
    stop("ZrodloDanychODBC nie jest ciągiem znaków.")
  }
  if(!is.data.frame(parametry)){
    stop("Argument 'parametry' nie jest ramką danych.")
  }
  if(is.null(parametry$typ)){
    stop("Nie określony typ parametrów.")
  }
  if(is.null(parametry$wartosc)){
    stop("Nie określona wartość parametrów.")
  }
  if(is.null(parametry$zmienna1)){
    stop("Nie określona kolumna 'zmienna1' dla ramki parametry.")
  }
  if(is.null(parametry$zmienna2)){
    stop("Nie określona kolumna 'zmienna2' dla ramki parametry.")
  }
  if(is.null(parametry$'S.E.')){
    stop("Nie określona kolumna 'S.E.' dla ramki parametry.")
  }
  if( !is.numeric(parametry$wartosc)){
    stop("Kolumna 'wartość' nie jest ciągiem liczb. ")
  }
  if( !is.numeric(parametry$'S.E.')){
    stop("Kolumna 'S.E.' nie jest ciągiem liczb. ")
  }

  stop("Funkcja wymaga przepisania")
#   tryCatch({
#     P = odbcConnect(zrodloDanychODBC)
#     # zapytanie
#     if(!is.null(nazwa_skali)){
#       zapytanie = "SELECT id_skali FROM skale where nazwa = ? "
#       if(!is.null(id_testu)){
#         zapytanie = paste0(zapytanie, " AND id_testu = ? ")
#       }
#     } else{
#       zapytanie = "SELECT id_skali FROM skale WHERE id_testu =  ? "
#     }
#
#     if( !is.null(nazwa_skali) & is.null(id_testu) ){
#       skaleZap = sqlExecute(P, zapytanie, data = nazwa_skali, fetch = TRUE)
#     } else if( !is.null(nazwa_skali) & ! is.null(id_testu)    ){
#       skaleZap = sqlExecute(P, zapytanie, data = data.frame(nazwa_skali, id_testu), fetch = TRUE)
#     } else{
#       skaleZap = sqlExecute(P, zapytanie, data = id_testu, fetch = TRUE)
#     }
#
#     if( length(na.omit(skaleZap))==0 ){
#       odbcClose(P)
#       stop("Nie ma skali w bazie danych.")
#     } else if(length(skaleZap)!=1){
#       odbcClose(P)
#       stop("Skala określona niejednoznacznie.")
#     }
#     idSkali = skaleZap$id_skali[1]
#
#     estymacjeNumZap = sqlExecute(P, "SELECT count(*) FROM sl_estymacje_parametrow WHERE estymacja = ? ",
#                                  data=estymacja, fetch=TRUE)
#
#     nazwyParametrow = sqlExecute(P, "SELECT parametr FROM sl_parametry", data=data.frame(NULL), fetch=TRUE)$parametr
#
#     zapytanie = "SELECT greatest(max(skalowanie), count(*)) + 1 AS skalowanie
#     FROM skalowania WHERE id_skali = ? "
#     numerSkalowania = sqlExecute(P, zapytanie, data=idSkali, fetch=TRUE)$skalowanie
#
#     odbcClose(P)
#   },
#   error=function(e){
#     odbcClose(P)
#     stop(e)
#   }
#   )
#   if( estymacjeNumZap == 0 ){
#     stop("Podanej metody estymacji nie ma w bazie danych.")
#   }
#
#   P = odbcConnect(zrodloDanychODBC)
#   tryCatch({
#     parBy = parametry[parametry$typ=="by", ]
#     parTreshold = parametry[parametry$typ=="threshold", ]
#
#     by = wydziel_kryteria_pseudokryteria(parBy$zmienna2)
#     kryteriaBy = by$kryteria
#     pseudokryteriaBy = by$pseudokryteria
#
#     tres = wydziel_kryteria_pseudokryteria(parTreshold$zmienna1)
#     kryteriaTres = tres$kryteria
#     pseudokryteriaTres = tres$pseudokryteria
#
#     liczbaParamKryteria = table(kryteriaTres)
#     liczbaParamPseudok = table(pseudokryteriaTres)
#
#     liczbaParam = c(liczbaParamKryteria, liczbaParamPseudok)
#
#     zapytanie = "SELECT kolejnosc, id_kryterium, id_pseudokryterium FROM skale_elementy WHERE id_skali = ?"
#     skaleElementy =  sqlExecute(P, zapytanie, data=idSkali, fetch=TRUE)
#
#     kryteriaBaza = na.omit(skaleElementy$id_kryterium)
#     pseudokryteriaBaza = na.omit(skaleElementy$id_pseudokryterium)
#
#     sprawdz_zgodnosc_kryteriow(kryteriaTres, kryteriaBy  , "kryteria 'treshold'" , "kryteria 'by'")
#     sprawdz_zgodnosc_kryteriow(kryteriaBy  , kryteriaTres, "kryteria 'by'"       , "kryteria 'treshold'")
#     sprawdz_zgodnosc_kryteriow(kryteriaTres, kryteriaBaza, "kryteria z parametru", "kryteria z bazy")
#     sprawdz_zgodnosc_kryteriow(kryteriaBaza, kryteriaTres, "kryteria z bazy"     , "kryteria z parametru", error=FALSE)
#
#     sprawdz_zgodnosc_kryteriow(pseudokryteriaTres, pseudokryteriaBy  , "pseudokryteria 'treshold'"         , "pseudokryteria 'by'")
#     sprawdz_zgodnosc_kryteriow(pseudokryteriaBy  , pseudokryteriaTres, "pseudokryteria 'by'"               , "pseudokryteria 'treshold'")
#     sprawdz_zgodnosc_kryteriow(pseudokryteriaTres, pseudokryteriaBaza, "pseudokryteria z parametru funkcji", "pseudokryteria z bazy")
#     sprawdz_zgodnosc_kryteriow(pseudokryteriaBaza, pseudokryteriaTres, "pseudokryteria z bazy"             , "pseudokryteria z parametru funkcji", error=FALSE)
#
#     odbcSetAutoCommit(P, FALSE)
#
#     # odbcEndTran(P, TRUE)
#     # odbcClose(P)
#
#     insert = "INSERT INTO skalowania (skalowanie, opis , estymacja, id_skali)  VALUES (? , ? , ?, ?)"
#     sqlExecute(P, insert , data=data.frame(numerSkalowania, opis, estymacja, idSkali))
#
#     for(k in seq_along(liczbaParamKryteria)){
#       # for(k in 1:10){
#       krytNum = as.numeric(names(liczbaParamKryteria)[k])
#
#       kolejnoscTemp = skaleElementy$kolejnosc[ (skaleElementy$id_kryterium == krytNum) %in% TRUE ]
#
#       parByNumeryKryteriow = suppressWarnings(as.numeric(gsub("^k_", "", parBy$zmienna2)))
#       parTresholdNumeryKryteriow = suppressWarnings(as.numeric(gsub("^k_", "", parTreshold$zmienna1)))
#
#       # model 2PL
#       if( liczbaParamKryteria[k]== 1 ){
#
#         wstaw_do_skalowania_elementy(P, idSkali, kolejnoscTemp, numerSkalowania,
#                                      "dyskryminacja", "2PL", parBy$wartosc[parByNumeryKryteriow %in% krytNum],
#                                      parBy$'S.E.'[parByNumeryKryteriow %in% krytNum])
#
#         wstaw_do_skalowania_elementy(P, idSkali, kolejnoscTemp, numerSkalowania,
#                                      "trudność", "2PL",
#                                      parTreshold$wartosc[parTresholdNumeryKryteriow %in% krytNum]/parBy$wartosc[parByNumeryKryteriow %in% krytNum],
#                                      parTreshold$'S.E.'[parTresholdNumeryKryteriow %in% krytNum]/parBy$wartosc[parByNumeryKryteriow %in% krytNum] )
#
#
#       } else if ( liczbaParamKryteria[k] > 1 ){ # model GRM
#         # dyskryminacja = parBy[parByNumeryKryteriow %in% krytNum, "wartosc"]
#         dyskryminacja =parBy$wartosc[parByNumeryKryteriow %in% krytNum]
#
#         wstaw_do_skalowania_elementy (P, idSkali, kolejnoscTemp,
#                                       numerSkalowania, "dyskryminacja",
#                                       "GRM", dyskryminacja, parBy$'S.E.'[parByNumeryKryteriow %in% krytNum])
#
#         indTres = which(parTresholdNumeryKryteriow %in% krytNum)
#         srednia = mean(parTreshold[indTres, "wartosc"]) / parBy$wartosc[parByNumeryKryteriow %in% krytNum]
#
#         wstaw_do_skalowania_elementy(P, idSkali, kolejnoscTemp, numerSkalowania,
#                                      "trudność", "GRM", srednia, NULL)
#
#         for(m in indTres ){
#           nazwaPar = paste0("b", bNr <- which(m==indTres))
#
#           if( ! nazwaPar %in% nazwyParametrow ){
#             insert = paste0("INSERT INTO sl_parametry(parametr, opis) values ( ? ,'trudność ", bNr, " poziomu wykonania')")
#             sqlExecute(P, insert, data = nazwaPar)
#           }
#
#           wstaw_do_skalowania_elementy(P, idSkali, kolejnoscTemp, numerSkalowania, nazwaPar, "GRM",
#                                        parTreshold$wartosc[m]/dyskryminacja - srednia,
#                                        parTreshold$'S.E.'[m]/dyskryminacja)
#         }
#       }
#     }
#
#
#     for(k in seq_along(liczbaParamPseudok)){
#       # for(k in 1:10){
#       pseudoNum = as.numeric(names(liczbaParamPseudok)[k])
#
#       kolejnoscTemp = skaleElementy$kolejnosc[ (skaleElementy$id_pseudokryterium == pseudoNum) %in% TRUE ]
#
#       parByNumeryPseudo = suppressWarnings(as.numeric(gsub("^p_", "", parBy$zmienna2)))
#       parTresholdNumeryPseudo = suppressWarnings(as.numeric(gsub("^p_", "", parTreshold$zmienna1)))
#
#       # model 2PL
#       if( liczbaParamPseudok[k]== 1 ){
#
#         wstaw_do_skalowania_elementy(P, idSkali, kolejnoscTemp, numerSkalowania,
#                                      "dyskryminacja", "2PL", parBy$wartosc[parByNumeryPseudo %in% pseudoNum],
#                                      parBy$'S.E.'[parByNumeryPseudo %in% pseudoNum])
#
#         wstaw_do_skalowania_elementy(P, idSkali, kolejnoscTemp, numerSkalowania,
#                                      "trudność", "2PL",
#                                      parTreshold$wartosc[parTresholdNumeryPseudo %in% pseudoNum]/parBy$wartosc[parByNumeryPseudo %in% pseudoNum],
#                                      parTreshold$'S.E.'[parTresholdNumeryPseudo %in% pseudoNum]/parBy$wartosc[parByNumeryPseudo %in% pseudoNum] )
#
#
#       } else if ( liczbaParamPseudok[k] > 1 ){ # model GRM
#         dyskryminacja = parBy[parByNumeryPseudo %in% pseudoNum, "wartosc"]
#
#         wstaw_do_skalowania_elementy (P, idSkali, kolejnoscTemp,
#                                       numerSkalowania, "dyskryminacja",
#                                       "GRM", dyskryminacja, parBy$'S.E.'[parByNumeryPseudo %in% pseudoNum])
#
#         indTres = which(parTresholdNumeryPseudo %in% pseudoNum)
#         srednia = mean(parTreshold[indTres, "wartosc"]) / dyskryminacja
#
#         wstaw_do_skalowania_elementy(P, idSkali, kolejnoscTemp, numerSkalowania,
#                                      "trudność", "GRM", srednia, NULL)
#
#         for(m in indTres ){
#           nazwaPar = paste0("b", bNr <- which(m==indTres))
#
#           if( ! nazwaPar %in% nazwyParametrow ){
#             insert = paste0("INSERT INTO sl_parametry(parametr, opis) values ( ? ,'trudność ", bNr, " poziomu wykonania')")
#             sqlExecute(P, insert, data = nazwaPar)
#           }
#
#           wstaw_do_skalowania_elementy(P, idSkali, kolejnoscTemp, numerSkalowania, nazwaPar, "GRM",
#                                        parTreshold$wartosc[m]/dyskryminacja - srednia,
#                                        parTreshold$'S.E.'[m]/dyskryminacja)
#         }
#       }
#     }
#
#     if(!is.null(rEAP)){
#       insert = "INSERT INTO skalowania_elementy  (id_elementu, id_skali,
#       skalowanie, model, parametr, wartosc)
#       VALUES (nextval('skalowania_elementy_id_elementu_seq'), ?, ?, 'n.d.', 'r EAP', ? )"
#
#       sqlExecute(P, insert,
#                  data = data.frame(idSkali, numerSkalowania, rEAP))
#     }
#
#     odbcEndTran(P, TRUE)
#     odbcClose(P)
#   },
#   error=function(e){
#     odbcEndTran(P, FALSE)
#     odbcClose(P)
#     stop(e)
#   }
#   )
#   return(numerSkalowania)
}
#' @title Wstawianie danych do tablicy 'skalowania_elementy'
#' @description
#' Funkcja wstawia jeden wiersz danych do tablicy 'skalowania_elementy'.
#' @param zrodloODBC źródło danych ODBC.
#' @param idSkali numer opisujący id skali
#' @param kolejnosc liczba
#' @param numerSkalowania liczba
#' @param nazwaParametru ciąg znaków określający nazwę parametru
#' @param model ciąg znaków
#' @param wartosc liczba
#' @param odchylenie liczba opisująca odchylenie standardowe parametru.
#' @return Funkcja nie zwraca żadnej wartości.
wstaw_do_skalowania_elementy = function(zrodloODBC, idSkali, kolejnosc, numerSkalowania,
                                        nazwaParametru, model, wartosc, odchylenie) {
  stop("Funkcja wymaga przepisania")
#   parametrGrupowy = sqlExecute(zrodloODBC, "select parametr, grupowy from sl_parametry where opis = ? or parametr = ?",
#                                data = data.frame(nazwaParametru, nazwaParametru), fetch=TRUE, stringsAsFactors = FALSE)
#
#   print(wartosc)
#   print(nazwaParametru)
#   print(parametrGrupowy)
#
#   insSkalowania = paste0("INSERT INTO skalowania_elementy  (id_elementu, id_skali, kolejnosc,
#                          skalowanie, parametr, ", ifelse(is.na(parametrGrupowy$grupowy), "","grupowy, "), " model, wartosc, uwagi", ifelse(is.null(odchylenie), "",", bl_std"), ")
#                          VALUES (nextval('skalowania_elementy_id_elementu_seq'), ?, ?, ?, ?, ?, ?,", ifelse(is.na(parametrGrupowy$grupowy),  "", " ?,"),
#                          " ''", ifelse(is.null(odchylenie), "", ", ?" ), ")"
#                          )
#
#
#   if(is.na(parametrGrupowy$grupowy)){
#     parametrGrupowy = parametrGrupowy$parametr
#   }
#
#   if(is.null(odchylenie)){
#     sqlExecute(zrodloODBC, insSkalowania,
#                data = data.frame(idSkali, kolejnosc, numerSkalowania, parametrGrupowy, model, wartosc))
#   } else{
#     sqlExecute(zrodloODBC, insSkalowania,
#                data = data.frame(idSkali, kolejnosc, numerSkalowania, parametrGrupowy, model, wartosc, odchylenie))
#   }
#   invisible(NULL)
}
#' @title Sprawdzanie zgodnosci kryteriow.
#' @description
#' Funkcja sprawdza, czy wektor kryteriów kryt1 jest zawarty w zbiorze kryteriów kryt2.
#' @param kryt1 wektor liczb opisujący kryteria
#' @param kryt2 wektor liczb opisujący kryteria
#' @param nazwa1 nazwa opisująca wektor kryt1
#' @param nazwa2 nazwa opisująca wektor kryt2
#' @param error jeżeli TRUE to funkcje wyświetla błąd w przypadku, gdy któryś z elementów kryt1
#' nie należy do kryt2.
#' @return Funkcja nie zwraca żadnej wartości.
sprawdz_zgodnosc_kryteriow = function(kryt1, kryt2, nazwa1, nazwa2, error=TRUE) {
  stop("Funkcja wymaga przepisania")
#   if( length(fInds = which( ! unique(kryt1) %in% unique(kryt2))) !=0  ){
#     if(error){
#       stop(paste0(nazwa1, " i ", nazwa2," nie pokrywają się.",
#                   " Brakujące ", nazwa1, ":\n"),
#            paste(unique(kryt1)[fInds], collapse="\n"))
#     } else {
#       warning(paste0(nazwa1, " i ", nazwa2, " nie pokrywają się.",
#                      " Brakujące ", nazwa1, ":\n"),
#               paste(unique(kryt1)[fInds], collapse="\n"))
#     }
#   }
#   invisible(NULL)
}
#' @title Wydzielanie kryteriow i pseudokryteriow
#' @description
#' Funkcja wydziala kryteria i pseudokryteria z wektora ciągów znakowych oraz sprawdza poprawność ich nazw.
#' @param nazwy wektor ciągów znakowych
#' @return Funkcja zwraca dwuelementową listę. Pierwszy element o nazwie kryteria zawiera wektor liczb,
#' który zawiera numery kryteriow. Drugi element to wektor numerów pseudokryteriów.
wydziel_kryteria_pseudokryteria = function(nazwy) {
  stop("Funkcja wymaga przepisania")
#   poprawneKrytBy = grepl("^k_[0-9]", nazwy)
#   poprawnePseudokrytBy = grepl("^p_[0-9]", nazwy)
#   poprawneIinneKryteriaBy =  grepl("^([[:alnum:]]+_)+[[:digit:]]+", nazwy)
#
#   if(sum(! poprawneIinneKryteriaBy) > 0){
#     stop("Nazwy nie pasujące do kryteriów i pseudokryteriów:\n",
#          paste(nazwy[! poprawneIinneKryteriaBy], collapse="\n" ))
#   }
#   if(sum(poprawneIinneKryteriaBy & ! poprawneKrytBy & ! poprawnePseudokrytBy) > 0){
#     warning("Niepoprawne nazwy kryteriów:\n",
#             paste(nazwy[poprawneIinneKryteriaBy & ! poprawneKrytBy & ! poprawnePseudokrytBy], collapse="\n" ))
#   }
#
#   numery = as.numeric( sub("^[[:alnum:]]+_", "", nazwy) )
#
#   kryteria = numery[poprawneIinneKryteriaBy & ! poprawnePseudokrytBy]
#   pseudokryteria = numery[poprawnePseudokrytBy]
#
#   return(list(kryteria=kryteria, pseudokryteria = pseudokryteria))
}
