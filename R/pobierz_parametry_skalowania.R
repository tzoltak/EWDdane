#' @title Pobieranie parametrow skalowan.
#' @description
#' Funkcja pobiera parametry skalowań o podanej nazwie skali i numerze testu oraz których
#' opis skalowań spełnia podane wyrażenie regularne.
#' @param nazwa_skali nazwa skali. Kiedy nazwa skali przymuje wartość NULL nie jest brana pod
#' uwagę w wyszukiwaniu. Wartość domyślna to NULL.
#' @param id_testu id testu.Kiedy id testu przymuje wartość NULL nie jest brana pod
#' uwagę w wyszukiwaniu. Wartość domyślna to NULL.
#' @param opis_skalowania wyrażenie regularne określające opis skalowania. Domyślna wartość to '\%'.
#' @param zrodloDanychODBC string określający źródło danych. docelowa wartość domyślna to 'EWD'. Obecnie 'ewd_grzes'.
#' @param parametryzacja parametr określający format zwracanego wyniku. Domyślna wartość to 'baza'.
#' Inna możliwa wartość to 'mplus'.
#' @return W przyopadku użycia parametryzacji 'baza', funkcja zwraca listę taką, że:
#' \itemize{
#'    \item każdy element listy opisuje parametry innego skalowania;
#'    \item każdy element listy ma też przypisane jako atrybuty wartości kolumn: 'skalowanie', 'opis', 'estymacja';
#'    \item zwracana lista ma przypisane jako atrybuty wartości wszystkich kolumn z tablicy 'skale';
#' }
#' W przyopadku użycia parametryzacji 'mplus' funkcja zwraca parametry skalowania w formie ramki danych, która jest
#' w postaci zwracanej przez funkcję skaluj().
#' @examples
#' \dontrun{
#' id_testu = 1128
#' nazwa_skali = "ktt;1128"
#' pobierz_parametry_skalowania(nazwa_skali, id_testu)
#' pobierz_parametry_skalowania(nazwa_skali, id_testu, parametryzacja = "mplus")
#' }
#' @import RODBCext
#' @export
pobierz_parametry_skalowania <- function(nazwa_skali=NULL, id_testu=NULL, opis_skalowania='.*',
                                         zrodloDanychODBC = 'EWD', parametryzacja = "baza"){
  
  if(!parametryzacja %in% c("baza","mplus")){
    stop("Niepoprawna wartość parametru 'parametryzacja': ",parametryzacja)
  }
  
  if(is.null(nazwa_skali) & is.null(id_testu)  ){
    stop("Nazwa skali oraz id testu nie mogą mieć jednocześnie wartości null.")
  }
  
  if(  !is.null(nazwa_skali)  & !is.character(nazwa_skali) ){
    stop("Nazwa skali nie jest ciągiem znaków.")
  }
  if( !is.null(id_testu)  & !is.numeric(id_testu) ){
    stop("Id testu nie jest liczbą.")
  }
  if( !is.character(opis_skalowania) ){
    stop("Opis_skalowania nie jest ciągiem znaków.")
  }
  if( !is.character(zrodloDanychODBC) ){
    stop("ZrodloDanychODBC nie jest ciągiem znaków.")
  }
  
  where = "where"
  if( !is.null(nazwa_skali) ){
    where = paste0(where, " nazwa = ? " )
    
    if(!is.null(id_testu)){
      where = paste0(where, " and id_testu = ? ")
    }
  } else {
    where = paste0(where, " id_testu = ? ")
  }
  where = paste0(where, " and SA.opis ~* ? ")
  
  #   joiny =             "
  #   from skale AS S
  #   JOIN skale_elementy AS SE USING(id_skali)
  #   JOIN skalowania_elementy AS SAE USING(id_skali, kolejnosc)
  #   JOIN skalowania AS SA USING(id_skali, skalowanie)
  #   "
  
  joiny =             "
  from skalowania AS SA
  join skalowania_elementy AS SAE using (id_skali, skalowanie)
  left join skale_elementy AS SE using (id_skali, kolejnosc)
  join skale AS S using(id_skali)
  "
  
  #   select id_kryterium, id_pseudokryterium, SAE.model, SAE.parametr, SAE.wartosc, SA.skalowanie, bl_std
  #   from skalowania AS SA
  #   join skalowania_elementy AS SAE using (id_skali, skalowanie)
  #   left join skale_elementy AS SE using (id_skali, kolejnosc)
  #   join skale AS S using(id_skali)
  #   where S.nazwa ='ewd;s;2010' and SA.opis = 'testowa estymacja11'
  #   order by parametr
  
  zapytanie1 = paste0("
                      select
                      id_kryterium, id_pseudokryterium,
                      SAE.model, SAE.parametr, SAE.wartosc, SA.skalowanie, bl_std
                      ", joiny , where, '\n ORDER BY skalowanie, kolejnosc, parametr')
  
  zapytanie2 = paste0("
                      select distinct
                      SA.skalowanie, SA.opis, SA.estymacja
                      ", joiny , where)
  
  zapytanie3 = paste0("
                      select distinct
                      S.*
                      ", joiny , where)
  
  
  if( !is.null(nazwa_skali) & !is.null(id_testu) ){
    sqlFrame = data.frame(nazwa_skali, id_testu, opis_skalowania)
  } else if( !is.null(nazwa_skali) & is.null(id_testu) ){
    sqlFrame = data.frame(nazwa_skali, opis_skalowania)
  } else  {
    sqlFrame = data.frame(id_testu, opis_skalowania)
  }
  
  
  tryCatch({
    P = odbcConnect(zrodloDanychODBC)
    tablicaDanych = sqlExecute(P, zapytanie1, data = sqlFrame, fetch = TRUE)
    opisSkalowan  = sqlExecute(P, zapytanie2, data = sqlFrame, fetch = TRUE)
    skale         = sqlExecute(P, zapytanie3, data = sqlFrame, fetch = TRUE)
    odbcClose(P)
  },
  error=function(e) {
    odbcClose(P)
    stop(e)
  }
  )
  
  if(nrow(tablicaDanych)==0){
    warning("Nie znalezniono danych spełniających kryteria wyszukiwania")
    return(NULL)
  }
  
  for(ind in 1:(ncol(tablicaDanych))){
    omitTmp = na.omit(as.character(tablicaDanych[,ind]))
    numbersTmp = suppressWarnings(as.numeric(omitTmp))
    if( length(numbersTmp)!=0 && all((!is.na(numbersTmp)))){
      if(is.null(attr(omitTmp,"na.action"))){
        tablicaDanych[,ind] = numbersTmp
      }else{
        tablicaDanych[,ind] = NA
        tablicaDanych[-attr(omitTmp,"na.action"),ind] = numbersTmp
      }
    }
  }
  
  ret = list()
  
  for(ind in 1:nrow(opisSkalowan)){
    ret[[ind]] = tablicaDanych[
      tablicaDanych[, "skalowanie"] == opisSkalowan[,ind],
      colnames(tablicaDanych) != "skalowanie"
      ]
    
    for(k in 2:ncol(opisSkalowan)){
      attr(ret[[ind]], colnames(opisSkalowan)[k]) = as.character(opisSkalowan[ind, k])
    }
  }
  
  if( nrow(skale) > 1 ){
    cat("Skale spełniające kryteria wyszukiwania: \n")
    print(skale)
    
    stop("Więcej niż jedna skala przypisana do wyników.")
  }
  
  for( k in 1:ncol(skale) ){
    attr(ret, colnames(skale)[k]) = as.character(skale[1, k])
  }
  
  if( parametryzacja=="mplus" ){
    for(ilist in seq_along(ret)){
      ret[[ilist]] = zmien_na_mplus(ret[[ilist]])
    }
  }
  return(ret)
}
#' @title Zmiana tablicy do formatu funkcji skaluj()
#' @description
#' Funkcja przekształca tablicę zwracaną w liście przez funkcję pobierz_parametry_skalowania() z parametrem 'baza'
#' do postaci mplusa
#' @param tablicaDanych tablica w formacie zwracanym przez funkcję pobierz_parametry_skalowania().
#' @return
#' Funkcja zwraca ramkę danych, która jest zgodna z postacią ramek zwracanych przez funkcję skaluj().
zmien_na_mplus <- function(tablicaDanych){
  
  opis = attributes(tablicaDanych)$opis
  estymacja = attributes(tablicaDanych)$estymacja
  
  grm = tablicaDanych[tablicaDanych$model=="GRM", ]
  dwaPL = tablicaDanych[tablicaDanych$model=="2PL",]
  
  if(nrow(grm)==0 && nrow(dwaPL)==0 ){
    stop("Brak parametrów modeli 2PL i GRM.")
  }
  
  # 2PL
  
  if( length ( errInds <- which( ! dwaPL$parametr %in% c("dyskryminacja","trudność")  ) ) != 0 ){
    stop("Niepoprawne rodzaje parametrów dla modelu 2PL: \n", paste(errInds, collapse="\n"))
  }
  
  # Jak rozumiem baza gwarantuje, że każdy element skale_elementy posiada kryterium albo pseudokryterium.
  kryt = dwaPL$id_kryterium
  kryt[is.na(kryt)] = dwaPL$id_pseudokryterium[is.na(kryt)]
  
  ret2PL = data.frame()
  for(krytNum in unique(kryt)){
    czyKryterium = krytNum %in% na.omit(dwaPL$id_kryterium)
    
    by = dwaPL$wartosc[dwaPL$id_kryterium == krytNum & dwaPL$parametr=="dyskryminacja" ]
    byStd = dwaPL$bl_std[dwaPL$id_kryterium == krytNum & dwaPL$parametr=="dyskryminacja" ]
    
    zmienna1 = ifelse(czyKryterium, "k", "p")
    zmienna2 = paste0(zmienna1, "_", krytNum)
    
    ret2PL = rbind(ret2PL, data.frame(typ="by", zmienna1, zmienna2, wartosc = by, S.E.= byStd ))
    
    tres = dwaPL$wartosc[dwaPL$id_kryterium == krytNum & dwaPL$parametr=="trudność" ]
    tresStd = dwaPL$bl_std[dwaPL$id_kryterium == krytNum & dwaPL$parametr=="trudność" ]
    
    ret2PL = rbind(ret2PL, data.frame(typ="treshold", zmienna1=zmienna2, zmienna2=zmienna1,
                                      wartosc = tres*by, S.E.=tresStd*by ))
  }
  
  #GRM
  kryt = grm$id_kryterium
  kryt[is.na(kryt)] = na.omit(grm$id_pseudokryterium)
  
  retGRM = data.frame()
  for(krytNum in unique(kryt)){
    
    czyKryterium = krytNum %in% tablicaDanych$id_kryterium
    
    by  = grm[ grm$parametr=="dyskryminacja" & kryt == krytNum, c("wartosc", "bl_std")]
    srednia = grm$wartosc[ grm$parametr=="trudność" & kryt == krytNum] * by$wartosc
    kPar = grm[ grepl("^k[[:digit:]+]$", grm$parametr) & kryt == krytNum , c("wartosc", "bl_std")]
    
    zmienna1 = ifelse(czyKryterium, "k", "p")
    zmienna2 = paste0(zmienna1, "_", krytNum)
    retGRM = rbind(retGRM,
                   data.frame(typ ='by', zmienna1=zmienna1, zmienna2=zmienna2,
                              wartosc = by$wartosc, S.E.= by$bl_std  ) )
    
    retGRM = rbind(retGRM,
                   data.frame(typ ='treshold', zmienna1=zmienna2, zmienna2=zmienna1,
                              wartosc = kPar$wartosc*by$wartosc + srednia, S.E.=kPar$bl_std*by$wartosc ) )
  }
  
  ret = rbind(ret2PL, retGRM)
  
  ret = ret[order(ret$typ, ret$zmienna1, ret$zmienna2), ]
  
  attributes(ret)$opis = opis
  attributes(ret)$estymacja = estymacja
  #r EAP
  attributes(ret)$'r EAP' = tablicaDanych$wartosc[tablicaDanych$parametr=='r EAP']
  
  return (ret)
}