#' @title Agregowanie wskaźników EWD
#' @description
#' Funkcja agreguje wskaźniki EWD.
#' @param dane ramka danych zwracana przez funkcję \link{pobierz_wartosci_wskaznikow_ewd}.
#' @param poziom ciąg znaków definiujący poziom agregacji: "gmina", "powiat" lub "województwo" lub NULL, 
#' jeśli podany został parametr 'funkcjaGrupujaca'
#' @param grupujPoLatach wartość logiczna (TRUE lub FALSE) wskazująca, czy przy wyróżnianiu grup oprócz TERYTu 
#' ma być uwzględniona również zmienna opisująca okres, dla którego wyliczony został wskaźnik
#' @param funkcjaGrupujaca fukcja przyjmująca jako pierwszy parametr teryt szkoły i zwracająca jednoelementowy obiekt.
#' która określa grupowanie wskaźników ewd. W zwracanej ramce danych przez agreguj_wskazniki_ewd() 
#' jedna kolumna będzie zawierać wynik funkcji grupującej (jeżeli grupujPoLatach=FALSE) lub sześć ostatnich cyfr wyniku tej funkcji (jeżeli grupujPoLatach=TRUE).
#' @param tylkoWyswietlane wartość logiczna (TRUE/FALSE/NA) opisująca, czy przy wyliczaniu agregatów mają zostać uwzględnione tylko te szkoły, których elipsy są pokazywane na stronie lub NA; przy TRUE uwzględniane są tylko te szkoły, których elipsy są wyświetlane; przy FALSE uwzględniane są wszystkie szkoły, bez względu na kategorię (ale z uwzględnieniem paramertu czyPomin); domyślna wartość NA jest tożsama z FALSE z jednym wyjątkiem: jeśli w danej grupie nie ma żadnej szkoły, której elipsa byłaby prezentowana, to wartość wskaźnika danej grupy jest ustawiana na NA.
#' @param paramGrupujaca lista parametrów dodatkowych funkcji grupującej.
#' @param czyPomin parametr logiczny. Jeżeli TRUE to w obliczeniach pomijane są szkoły specjalne, dla dorosłych i przyszpitalne.
#' @return data frame
#' @examples
#' \dontrun{
#' daneOrg = pobierz_wartosci_wskaznikow_ewd('T', c(2013, 2012), dodatkoweInfo = TRUE, jst = "^2605|^2601", opisoweNazwy = FALSE, tylkoWyswietlane = FALSE, tylkoWskDoPrezentacji = FALSE, czyPomin = FALSE)
#' jedenWiersz = daneOrg[1, ]
#' jedenWiersz$id_szkoly = 123
#' jedenWiersz$teryt_szkoly=123456
#' jedenWiersz$dla_doroslych= TRUE
#' jedenWiersz[, grepl("wyswietlaj_", colnames(jedenWiersz))][1] = 0
#' agreguj_wskazniki_ewd(jedenWiersz, 
#'                       poziom = "powiat", grupujPoLatach = FALSE, tylkoWyswietlane=NA, czyPomin = TRUE )
#' agreguj_wskazniki_ewd(jedenWiersz, 
#'                       poziom = "powiat", grupujPoLatach = FALSE, tylkoWyswietlane=FALSE, czyPomin = TRUE )
#' agreguj_wskazniki_ewd(jedenWiersz,
#'                       poziom = "powiat", grupujPoLatach = FALSE, tylkoWyswietlane=TRUE, czyPomin = TRUE )
#' 
#' agreguj_wskazniki_ewd(jedenWiersz, 
#'                       poziom = "powiat", grupujPoLatach = FALSE, tylkoWyswietlane=NA, czyPomin = FALSE )
#' agreguj_wskazniki_ewd(jedenWiersz, 
#'                       poziom = "powiat", grupujPoLatach = FALSE, tylkoWyswietlane=FALSE, czyPomin = FALSE )
#' agreguj_wskazniki_ewd(jedenWiersz,
#'                       poziom = "powiat", grupujPoLatach = FALSE, tylkoWyswietlane=TRUE, czyPomin = FALSE )
#' 
#' dane = rbind(daneOrg, jedenWiersz)
#' agreguj_wskazniki_ewd(dane, 
#'                       poziom = "powiat", grupujPoLatach = TRUE, tylkoWyswietlane=NA, czyPomin = TRUE )
#' agreguj_wskazniki_ewd(dane, 
#'                       poziom = "powiat", grupujPoLatach = TRUE, tylkoWyswietlane=FALSE, czyPomin = TRUE )
#' agreguj_wskazniki_ewd(dane,
#'                       poziom = "powiat", grupujPoLatach = TRUE, tylkoWyswietlane=TRUE, czyPomin = TRUE )
#' 
#' agreguj_wskazniki_ewd(dane, 
#'                       poziom = "powiat", grupujPoLatach = TRUE, tylkoWyswietlane=NA, czyPomin = FALSE )
#' agreguj_wskazniki_ewd(dane, 
#'                       poziom = "powiat", grupujPoLatach = TRUE, tylkoWyswietlane=FALSE, czyPomin = FALSE )
#' agreguj_wskazniki_ewd(dane,
#'                       poziom = "powiat", grupujPoLatach = TRUE, tylkoWyswietlane=TRUE, czyPomin = FALSE 
#' }
#' @import dplyr
#' @import lazyeval
#' @import ZPD
#' @export
agreguj_wskazniki_ewd <- function(dane, poziom=NULL, grupujPoLatach=TRUE, funkcjaGrupujaca=NULL, tylkoWyswietlane=NA, czyPomin=TRUE, paramGrupujaca = list()){
  
  stopifnot(!is.null(poziom) | !is.null(funkcjaGrupujaca),  poziom %in% c("gmina", "powiat", "województwo"))
  
  if(!is.null(funkcjaGrupujaca) & !is.null(poziom)   ){
    stop("Jeden z parametrów: poziom lub funkcja grupująca musi być nulem.")
  } else if(!is.null(poziom)){
    funkcjaGrupujaca = przygotuj_funkcje_grupujaca_teryt(poziom)
  } 
  
  if( "teryt_szkoly" %in% colnames(dane) ){
    
    if(grupujPoLatach){
      rok_do =  "rok_do"
    } else{
      rok_do = NULL
    }
    
    grupowanie = do.call(funkcjaGrupujaca, append(list(dane$teryt_szkoly), paramGrupujaca))
    if( !is.data.frame(grupowanie) |  ncol(grupowanie)!=1 | nrow(grupowanie) != nrow(dane)){
      stop("Funkcja grupująca powinna zwracać dokładnie jedną wartość.")
    }
    nazwa_grupowania = colnames(grupowanie)
    dane = cbind(dane %>% select_(~ -teryt_szkoly), grupowanie)
    
    if(is.na(tylkoWyswietlane)){
      dots = paste0("list(~", paste0(c(rok_do, nazwa_grupowania), collapse = ", ~"), ")") %>% as.lazy() %>% lazy_eval()
      caleGrupowanie = dane %>% select_(.dots=dots) %>% distinct()
    }
    
    #Jeżeli czyPomin true to usuń szkoły specjalne.
    if(czyPomin){
      dane = dane[!dane$dla_doroslych & !dane$specjalna & !dane$przyszpitalna, ]
    }
    dane = dane[, grepl("^id_szkoly$|^teryt|^ewd|^lu_ewd|^rok_do$|^wyswietlaj_|^srednia_", tolower(colnames(dane)))]
    
    if(nrow(dane)==0 & is.na(tylkoWyswietlane)){
      ret = rbind_list(dane[, grepl("^ewd|^lu_ewd|^srednia_", tolower(colnames(dane)))], caleGrupowanie) %>% as.data.frame()
      return(ret)
    } else if(nrow(dane)==0){
      return(NULL)
    }
    
    zmienne = c("id_szkoly", "rok_do", nazwa_grupowania)
    
    daneDlugie_ewd = melt(dane[, c(zmienne, colnames(dane)[grepl("^ewd_|^lu_ewd_|^wyswietlaj_|^srednia_", colnames(dane))])],  id = zmienne)
    daneDlugie_ewd = cbind(daneDlugie_ewd, wskaznik = gsub("^ewd_|^lu_ewd_|^wyswietlaj_|^srednia_", "", daneDlugie_ewd$variable))
    daneDlugie_ewd$variable = gsub("mt(mp|m|p|h)_tl_wgr", "", daneDlugie_ewd$variable)
    
    if( !all( errInd <- unique(daneDlugie_ewd$variable) %in%  c("ewd_", "lu_ewd_", "wyswietlaj_", "srednia_"))   ){
      stop("Nierozpoznane nazwy kolumn związanych ze wskaźnikami:", unique(daneDlugie_ewd$variable)[!errInd])
    }
    
    dots_group_by = paste0("list(~", paste0(c(rok_do, nazwa_grupowania, "wskaznik"), collapse = ", ~"), ")") %>% 
      as.lazy() %>% lazy_eval()
    daneRozsadne = dcast(daneDlugie_ewd, as.formula(paste0( paste0(zmienne, collapse = "+") ," + wskaznik ~ variable")), value.var = "value") %>%
      group_by_(.dots=dots_group_by)
    
    # Gdy tylkoWyswietlane to TRUE to odfiltruj niewyświetlane.
    if(!is.na(tylkoWyswietlane) & tylkoWyswietlane){
      daneRozsadne = daneRozsadne %>% filter_(~ wyswietlaj_==1)
    }
    
    danePogrupowane = daneRozsadne %>% 
      summarise_(ewd_ =  interp(~weighted.mean(ewd_, lu_ewd_)),  
                 srednia_ = interp(~weighted.mean(srednia_, lu_ewd_)), 
                 lu_ewd_ = interp(~sum(lu_ewd_)))
    
    srednie = dcast(danePogrupowane, as.formula(paste0( paste0(c(rok_do, nazwa_grupowania), collapse = "+") ," ~ wskaznik")), value.var = "srednia_")
    colnames(srednie)[grepl("mt(mp|m|p|h)_tl_wgr", colnames(srednie))] = paste0("srednia_", colnames(srednie)[grepl("mt(mp|m|p|h)_tl_wgr", colnames(srednie))])
    
    ewd = dcast(danePogrupowane, as.formula(paste0( paste0(c(rok_do, nazwa_grupowania), collapse = "+") ," ~ wskaznik")), value.var = "ewd_")
    colnames(ewd)[grepl("mt(mp|m|p|h)_tl_wgr", colnames(ewd))] = paste0("ewd_", colnames(ewd)[grepl("mt(mp|m|p|h)_tl_wgr", colnames(ewd))])
    
    lu_ewd = dcast(danePogrupowane, as.formula(paste0( paste0(c(rok_do, nazwa_grupowania), collapse = "+") ," ~ wskaznik")), value.var = "lu_ewd_") 
    colnames(lu_ewd)[grepl("mt(mp|m|p|h)_tl_wgr", colnames(lu_ewd))] = paste0("lu_ewd_", colnames(lu_ewd)[grepl("mt(mp|m|p|h)_tl_wgr", colnames(lu_ewd))])
    
    ret = suppressMessages(ewd %>% inner_join(srednie) %>% inner_join(lu_ewd))
    # Dołączenie grup bez szkół, gdy tylkoWyswietlane wynosi NA.
    if(is.na(tylkoWyswietlane)){
      ret = suppressMessages(left_join(caleGrupowanie, ret))
    }
    
  } else if ("TERYT gminy" %in% colnames(dane) ){
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
#' @return funkcja
przygotuj_funkcje_grupujaca_teryt <- function(poziom){
  stopifnot(poziom %in% c("gmina", "powiat", "województwo"))
  fun = switch(poziom,
               gmina = function(teryt){
                 return(data.frame(teryt_gminy=teryt))
               },
               powiat = function(teryt){
                 return(data.frame(teryt_powiatu=round(teryt/100)*100))
               },
               województwo = function(teryt){
                 return(data.frame(teryt_województwa=round(teryt/10000)*10000))
               }
  )
  return(fun)
}