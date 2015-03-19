#' @title Pobieranie danych maturalnych
#' @description
#' Funkcja pobiera z bazy dane maturalne dla wybranych roczników.
#' @param src uchwyt źródła danych dplyr-a do bazy EWD.
#' szkoły zwrócone zostaną tylko najświeższe dane w ramach tego okresu)
#' @param rok_mat wektor lat egzaminu
#' @param demo domyślnie FALSE. Jeżeli TRUE to pobiera tylko trzy konkretne rekordy (zniknie z docelowej wersji). 
#' @return data frame
 #'@examples
#'\dontrun{
#' src = polacz(user = "user", password = "pass")
#' ret = pobierz_dane_maturalne(src, 2012:2014, TRUE)
#'}
#' @import dplyr
#' @import ZPD
#' @export
pobierz_dane_maturalne <- function(src, rok_mat, demo=FALSE){
  
  powiazania = list(
    "mp" = "matura;polski",
    "mm" =  "matura;matematyka",
    "mh" = "matura;humanistyczna",
    "mmp" = "matura;matematyczno-przyrodnicza"
  )
  
  uczniowie      = pobierz_uczniow(src, daneOsobowe = TRUE)  %>% select(id_obserwacji, plec, data_ur)
  szkoly         = pobierz_szkoly(src) %>% filter(rok %in% rok_mat, !is.na(typ_szkoly) ) %>% 
    select(id_szkoly, typ_szkoly, publiczna, dla_doroslych, specjalna, przyszpitalna, matura_miedzynarodowa)
  
  ostatniRok = max(rok_mat)
  szkolyOstatnieDane  = pobierz_szkoly(src) %>% filter(rok == ostatniRok, !is.na(typ_szkoly) ) %>% 
    select(id_szkoly, teryt_szkoly, wielkosc_miejscowosci) %>% distinct()
  
  szkoly = inner_join(szkoly, szkolyOstatnieDane) %>% distinct()
  
  uczniowieTesty = pobierz_dane_uczniowie_testy(src, daneOsobowe = TRUE) %>% filter(klasa %!~% "^[[:digit:]]$") %>%
    select(id_obserwacji, id_testu, oke, id_szkoly, dysleksja, laureat, klasa, pop_podejscie)
  
  if(demo){
    testowe_obserwacje =  c(2373787, 2056141, 1829739)
    uczniowieTesty = uczniowieTesty %>% filter(id_obserwacji %in% testowe_obserwacje)
  }
  
  testy          = pobierz_testy(src) %>% filter(rodzaj_egzaminu == "matura", rok %in% rok_mat)
  dysLaurRokSzk  = inner_join(uczniowieTesty, testy)   
  dysLaurRok     = dysLaurRokSzk %>% group_by(id_obserwacji, klasa, pop_podejscie, dysleksja, laureat) %>%
    summarise(data_testu = min(data_testu)) %>% distinct()
  
  ids_obs        = dysLaurRokSzk %>% select(id_obserwacji) %>% distinct()
  ids_szkol      = dysLaurRokSzk %>% select(id_szkoly) %>% distinct()
  
  oszacowania    = pobierz_oszacowania_uczniow(src)  %>%  filter(estymacja == "EAP")
  oszacowania    = inner_join(oszacowania, ids_obs)
  
  szablonOpisu   = paste0(rok_mat, collapse = "|")
  testy          = pobierz_testy(src) %>% 
    filter(dane_ewd == TRUE, opis_testu %~% "matura", opis_testu %~% szablonOpisu) %>% select(id_testu, arkusz, rodzaj_egzaminu, czesc_egzaminu, opis_testu) # %>% filter(opis_testu %~% "matura", opis_testu %~% szablonOpisu)
  
  oszacTestyMat   = inner_join(oszacowania, testy) %>% 
    select(id_obserwacji, id_testu, wynik, id_szkoly, opis_testu) %>% as.data.frame()
  
  head(oszacTestyMat)
  
  ret = data.frame(id_obserwacji = numeric(0), id_szkoly= numeric(0), rok=numeric(0))
  for(i in seq_along(powiazania)){
    tmp = oszacTestyMat %>% filter(grepl(powiazania[[i]], opis_testu))
    tmp$rok = as.numeric(gsub(paste0(powiazania[[i]], ";"), "", tmp$opis_testu))
    names(tmp)[grepl("^id_testu$|^wynik$|^opis_testu$", names(tmp))  ]   = paste0(c("id_testu_","irt_", "opis_testu_"), names(powiazania)[i])    
    ret= full_join(tmp, ret)  
  }
  oszacTestyMat = ret
  
  uczniowie = left_join(uczniowie, dysLaurRok)  %>% inner_join(ids_obs) %>% as.data.frame()
  utso = inner_join(uczniowie, oszacTestyMat)
  szkoly = szkoly %>% inner_join(ids_szkol) %>% as.data.frame()
  
  return(inner_join(utso, szkoly)) 
}
#' @title Pobieranie danych gimnazjalnych
#' @description
#' Funkcja pobiera z bazy wyskalowane dane gimnazjalne dla wybranych roczników.
#' @param src uchwyt źródła danych dplyr-a do bazy EWD.
#' szkoły zwrócone zostaną tylko najświeższe dane w ramach tego okresu)
#' @param rok_gim wektor lat egzaminu
#' @param demo domyślnie FALSE. Jeżeli TRUE to pobiera tylko trzy konkretne rekordy (zniknie z docelowej wersji). 
#' @return data frame
#'@examples
#'\dontrun{
#' src = polacz(user = "user", password = "pass")
#' rok_matury = 2012:2014
#' wydl=1
#' rok_gim = (min(rok_matury)-3-wydl):(max(rok_matury)-3)
#' ret = pobierz_dane_gimnazjalne(src, rok_gim, TRUE)
#'}
#' @import dplyr
#' @import ZPD
#' @export
pobierz_dane_gimnazjalne <- function(src, rok_gim, demo=FALSE){
  
  powiazania = list(
    "gm" = "matematyczno-przyrodnicza",
    "gh" =  "humanistyczna"
  )
  
  rok_gim = c(0, rok_gim) # obejście błędu dplyr-a z translacją na SQL warunków "zmienna %in% x", gdzie x ma długość 1
  
  uczniowie      = pobierz_uczniow(src, daneOsobowe = TRUE)  %>% select_('id_obserwacji', 'plec', 'data_ur')
  szkoly         = pobierz_szkoly(src) %>% filter_(~rok %in% rok_gim, ~!is.na(typ_szkoly) ) %>% 
    select_('id_szkoly', 'typ_szkoly', 'publiczna', 'dla_doroslych', 'specjalna', 'przyszpitalna', 'matura_miedzynarodowa')
  
  ostatniRok = max(rok_gim)
  szkolyOstatnieDane  = pobierz_szkoly(src) %>% filter_(~rok == ostatniRok, ~!is.na(typ_szkoly) ) %>% 
    select_('id_szkoly', 'teryt_szkoly', 'wielkosc_miejscowosci') %>% distinct()
  
  
  szkoly = inner_join(szkoly, szkolyOstatnieDane) %>% distinct()
  
  uczniowieTesty = pobierz_dane_uczniowie_testy(src, daneOsobowe = TRUE) %>% # filter_(~klasa %!~% "^[[:digit:]]$") %>%
    select_('id_obserwacji', 'id_testu', 'oke', 'id_szkoly', 'dysleksja', 'laureat', 'klasa', 'pop_podejscie', 'kod_u')   
  
  if(demo){
    testowe_obserwacje = c(2373787, 2056141, 1829739)
    uczniowieTesty = uczniowieTesty  %>% filter_(~id_obserwacji %in% testowe_obserwacje)
  }
  
  testy          = pobierz_testy(src)  %>% filter_(~rodzaj_egzaminu == "egzamin gimnazjalny" , ~rok %in% rok_gim)
  dysLaurRokSzk  = inner_join(uczniowieTesty, testy)  
  
  dysLaurRok     = dysLaurRokSzk %>% group_by_('id_obserwacji', 'klasa', 'pop_podejscie', 'dysleksja', 'kod_u') %>%
    summarise(data_testu = min(data_testu)) %>% distinct()
  
  ids_obs        = dysLaurRokSzk %>% select_('id_obserwacji') %>% distinct()
  ids_szkol      = dysLaurRokSzk %>% select_('id_szkoly') %>% distinct()
  
  oszacowania    = pobierz_oszacowania_uczniow(src)   %>%  filter_(~estymacja == "EAP")
  oszacowania    = inner_join(oszacowania, ids_obs)
  
  testy          = pobierz_testy(src) %>% filter_(~rodzaj_egzaminu == "egzamin gimnazjalny", ~rok %in% rok_gim) %>%
    select_('id_testu', 'arkusz', 'rodzaj_egzaminu', 'czesc_egzaminu', 'opis_testu')
  
  skale = pobierz_skale(src, doPrezentacji = TRUE) %>% filter_(~rok %in% rok_gim, ~rodzaj_skali == "ewd", ~rodzaj_egzaminu == "egzamin gimnazjalny")  %>% 
    select_('id_skali', 'id_testu', 'rok', 'skalowanie', 'nazwa_skali')
  skaleTesty = skale %>% select_('id_skali', 'id_testu', 'nazwa_skali') 
  testy = inner_join(testy, skale)
  
  oszacTestyEgzaminy   = inner_join(oszacowania, testy) %>%
    select_('id_obserwacji', 'id_testu', 'wynik', 'bs', 'opis_testu', 'id_szkoly', 'czesc_egzaminu', 'arkusz',
            'id_skali', 'rodzaj_egzaminu', 'czesc_egzaminu', 'rok', 'nazwa_skali') 
  
  idTestow  = oszacTestyEgzaminy %>% select_('id_testu') %>% distinct()
  
  oceny = tbl(src, sql("select id_obserwacji, id_testu, ocena from odpowiedzi"))
  oceny = inner_join(oceny, idTestow) %>% inner_join(ids_obs) %>% group_by_('id_obserwacji', 'id_testu') %>% summarise(ocena = sum(ocena)) 
  normy = tbl(src, sql("select id_skali, wartosc as ocena, wartosc_zr as norm from normy_ekwikwantylowe"))
  
  oceny =  left_join(oceny, skaleTesty) %>% left_join(normy)
  
  oszacTestyMat = left_join(oszacTestyEgzaminy, oceny) %>%  inner_join(uczniowieTesty %>% select_('id_testu', 'id_obserwacji', 'laureat'))  %>% select(-id_skali, -rodzaj_egzaminu) %>% as.data.frame()
  
  ret = data.frame(id_obserwacji = numeric(0), id_szkoly = numeric(0), rok = numeric(0))
  for(i in seq_along(powiazania)){
    tmp = oszacTestyMat %>% filter(grepl(powiazania[[i]], czesc_egzaminu))
    names(tmp)[grepl("^id_testu$", names(tmp))  ] = paste0("id_testu_", names(powiazania)[i])
    names(tmp)[grepl("^wynik$", names(tmp))  ] = paste0("irt_", names(powiazania)[i])
    names(tmp)[grepl("^bs$", names(tmp))  ] = paste0("irt_bs_", names(powiazania)[i])
    names(tmp)[grepl("^opis_testu$", names(tmp))  ] = paste0("opis_testu_", names(powiazania)[i])
    names(tmp)[grepl("^arkusz$", names(tmp))  ] = paste0("arkusz_", names(powiazania)[i])
    names(tmp)[grepl("^ocena$", names(tmp))  ] = paste0("sum_", names(powiazania)[i])
    names(tmp)[grepl("^norm$", names(tmp))  ] = paste0("norm_", names(powiazania)[i])
    names(tmp)[grepl("^laureat$", names(tmp))  ] = paste0("laureat_", names(powiazania)[i])
    # names(tmp)[grepl("^id_testu$|^wynik$|^bs|^opis_testu$|^arkusz$", names(tmp))  ]   = paste0(c("id_testu_","irt_", "irt_bs_", "opis_testu_", "arkusz_"), names(powiazania)[i])    
    ret= full_join(tmp %>% select(-czesc_egzaminu), ret)  
  }
  oszacTestyMat = ret
  
  uczniowie = left_join(uczniowie, dysLaurRok)  %>% inner_join(ids_obs) %>% collect()
  utso = inner_join(uczniowie, oszacTestyMat)
  
  luSzkoly = pobierz_dane_uczniowie_testy(src, daneOsobowe = TRUE) %>% inner_join(ids_szkol) %>% select(id_obserwacji, rok, id_szkoly) %>% distinct() %>% 
    group_by(rok, id_szkoly)  %>% summarise(lu = n())
  szkoly = szkoly %>% inner_join(ids_szkol) %>% left_join(luSzkoly) %>% collect()
  
  ret = inner_join(utso, szkoly) %>% as.data.frame()
  
}