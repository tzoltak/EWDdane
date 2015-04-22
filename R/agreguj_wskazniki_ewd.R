#' @title Agregowanie wskaźników EWD
#' @description
#' Funkcja agreguje wskaźniki EWD.
#' @param dane ramka danych zwracana przez funkcję \link{pobierz_wartosci_wskaznikow_ewd}.
#' @param poziom ciąg znaków definiujący poziom agregacji: "gmina", "powiat" lub "województwo" lub NULL, 
#' jeśli podany został parametr 'funkcjaGrupujaca'
#' @param grupujPoLatach wartość logiczna (TRUE lub FALSE) wskazująca, czy przy wyróżnianiu grup oprócz TERYTu 
#' ma być uwzględniona również zmienna opisująca okres, dla którego wyliczony został wskaźnik
#' @param funkcjaGrupujaca fukcja przyjmująca dwa parametry: teryt oraz rok_do egzaminu, a zwracająca jednokolumnową całkowitoliczbową ramkę danych,
#' która określa grupowanie wskaźników ewd. W zwracanej ramce danych przez agreguj_wskazniki_ewd() 
#' jedna kolumna będzie zawierać wynik funkcji grupującej (jeżeli grupujPoLatach=FALSE) lub sześć ostatnich cyfr wyniku tej funkcji (jeżeli grupujPoLatach=TRUE).
#' @param tylkoWyswietlane parametr nie oprogramowany
#' @param czyPomin parametr nie oprogramowany
#' @return data frame
#' @examples
#' \dontrun{
#' dane = pobierz_wartosci_wskaznikow_ewd('T', c(2013, 2012), dodatkoweInfo = FALSE, jst = "^2605|^2601", opisoweNazwy = FALSE, tylkoWyswietlane = FALSE, tylkoWskDoPrezentacji = FALSE)
#' agreguj_wskazniki_ewd(dane, "powiat", grupujPoLatach = FALSE)
#' agreguj_wskazniki_ewd(dane, "powiat", grupujPoLatach = TRUE)
#' }
#' @import dplyr
#' @import lazyeval
#' @import ZPD
#' @export
agreguj_wskazniki_ewd <- function(dane, poziom = NULL, grupujPoLatach=TRUE, funkcjaGrupujaca=NULL, tylkoWyswietlane=NA, czyPomin=TRUE){
  
  stopifnot(!is.null(poziom) | !is.null(funkcjaGrupujaca),  poziom %in% c("gmina", "powiat", "województwo"))
  
  if(!is.null(funkcjaGrupujaca) & !is.null(poziom)   ){
    stop("Jeden z parametrów: poziom lub funkcja grupująca musi być nulem.")
  } else if(!is.null(poziom)){
    funkcjaGrupujaca = funkcja_grupujaca_teryt(poziom, grupujPoLatach)
  } 
    
  if(length(formals(funkcjaGrupujaca))!=2){
    stop("Funkcja nie przyjmuje dokładnie dwóch parametrów.")
  }
  
  if( nazwa_wskaznika %in% colnames(dane) ){
    daneSzkol = dane[, grepl("teryt|^ewd|^lu|^rok_do$", tolower(colnames(dane)))]
    grupowanie = with(daneSzkol, funkcjaGrupujaca(teryt_szkoly, rok_do))
    
    if(ncol(grupowanie)!=1 | !all(grupowanie%%1 == 0)){
      stop("Funkcja grupująca powinna zwracać tylko jedną kolumnę zawierającą liczby całkowite.")
    }
    
    daneSzkol = cbind(daneSzkol %>% select(-teryt_szkoly), grupowanie)  %>% group_by_(.dots=interp(~grupowanie, grupowanie = as.name(colnames(grupowanie))))
    
    zgupowane = daneSzkol %>% select_(.dots=interp(~grupowanie, grupowanie = as.name(colnames(grupowanie)))) %>% distinct()
    for( nazwa_wskaznika in colnames(daneSzkol)[grepl("^ewd", colnames(daneSzkol))]){

      srednieWazone = daneSzkol %>% summarise_(.dots = interp(~weighted.mean(ewd, lu_ewd), ewd = as.name(nazwa_wskaznika), lu_ewd = as.name(paste0("lu_", nazwa_wskaznika))))
      colnames(srednieWazone)[grepl("^weighted.mean", colnames(srednieWazone))] = nazwa_wskaznika
      liczbaUczniow = daneSzkol %>% summarise_(.dots = interp(~sum(lu_ewd), lu_ewd = as.name(paste0("lu_", nazwa_wskaznika))))
      colnames(liczbaUczniow)[grepl("^sum", colnames(liczbaUczniow))] = paste0("lu_", nazwa_wskaznika)
      
      zgupowane = suppressMessages(inner_join(srednieWazone, liczbaUczniow) %>% right_join(zgupowane))
    }
    
    if(grupujPoLatach){
      
      teryt_fun <- function(grupowanie){
        return(grupowanie%%1e6)
      }
      
      ret = daneSzkol %>% ungroup() %>% select_(~rok_do, .dots=interp(~grupowanie, grupowanie = as.name(colnames(grupowanie)))) %>% distinct() %>% 
                               mutate_(grupowanie = interp(~ teryt_fun(gr),  gr = as.name(colnames(grupowanie)))) 
      
      ret = suppressMessages(ret %>% inner_join(zgupowane))  %>% select_(.dots=interp(~-grupowanie, grupowanie = as.name(colnames(grupowanie)))) 
      colnames(ret)[grepl("^grupowanie$", colnames(ret))] = colnames(grupowanie)
      
    } else{
      ret = zgupowane
    }
    
  }
  else if ("TERYT gminy" %in% colnames(dane) ){
    stop("Każ Grześkowi dokończyć funkcję.")
  } else{
    stop("Funkcja nie zawiera zmiennej opisującej teryt")
  }
  
  return(ret %>% as.data.frame())
}
#' @title Przygotowanie funkcji grupującej teryt.
#' @description
#' Funkcja przygotowuje funkcję grupującą. Więcej szczegółów w \link{agreguj_wskazniki_ewd}
#' @param poziom ciąg znaków definiujący poziom agregacji: "gmina", "powiat" lub "województwo" lub NULL, 
#' jeśli podany został parametr 'funkcjaGrupujaca'
#' @param grupujPoLatach wartość logiczna (TRUE lub FALSE) wskazująca, czy przy wyróżnianiu grup oprócz TERYTu 
#' ma być uwzględniona również zmienna opisująca okres, dla którego wyliczony został wskaźnik
#' @return funkcja
przygotuj_funkcje_grupujaca_teryt <- function(poziom, grupujPoLatach){
  stopifnot(poziom %in% c("gmina", "powiat", "województwo"))
  
  if(!grupujPoLatach){
    fun = switch(poziom,
                 gmina = function(teryt, rok){
                   return(teryt)
                 },
                 powiat = function(teryt, rok){
                   return(data.frame(teryt_powiatu=round(teryt/100)*100))
                 },
                 województwo = function(teryt, rok){
                   return(round(teryt/10000)*10000)
                 }
    )
  } else{
    fun = switch(poziom,
                 gmina = function(teryt, rok){
                   return(teryt + rok*1e6)
                 },
                 powiat = function(teryt, rok){
                   return(data.frame(teryt_powiatu=round(teryt/100)*100+ rok*1e6))
                 },
                 województwo = function(teryt, rok){
                   return(round(teryt/10000)*10000+rok*1e6)
                 }
    )
  }
  return(fun)
}