pobierz_dane_szkol <- function(rok, typySzkol = NULL, idOke = FALSE, dolaczPaou = FALSE, zrodloODBC="EWD"){
  Sys.setlocale("LC_ALL", "pl_PL.UTF-8")
  
  P = odbcConnect(zrodloODBC)
  
  zapytanie = paste0( "SELECT SZ.*,
                      D.nazwa, D.adres, D.miejscowosc, D.pna,",
                      ifelse(idOke, "", "D.id_szkoly_oke,"),
                      "D.wielkosc_miejscowosci, id_gminy + 100*id_powiatu + 10000*id_wojewodztwa as TERYT",
                      ifelse( is.null(typySzkol) | any(typySzkol %in% c("LO","LP","T")) ,", D.matura_miedzynarodowa",""),
                      " FROM szkoly as SZ
                      JOIN szkoly_dane as D USING(id_szkoly)
                      where SZ.id_szkoly > 0 and
                      D.rok = ? ",
                      ifelse( is.null(typySzkol), "", "and SZ.typ_szkoly in (?) "),
                      ifelse( dolaczPaou, "", "and D.paou = FALSE"))
  
  dane = if( is.null(typySzkol)){
    list(rok)
  } else{
    list(rok, typySzkol)
  } 
  
  ret =  sqlExecute(P, zapytanie, fetch = TRUE, stringsAsFactors = FALSE, data=dane)
  odbcClose(P)
  
  ret$typ_szkoly[ret$typ_szkoly=="TRUE"] = "T"
  return( ret )
}

pobierz_dane_uczniow <- function(idtestow,  zrodloODBC="EWD"){

  Sys.setlocale("LC_ALL", "pl_PL.UTF-8")
  
  P = odbcConnect(zrodloODBC)
  
  zapytanie = paste0( "select distinct AR.rodzaj_egzaminu, EXTRACT(YEAR FROM AR.data_egzaminu)
                      from testy as T
                      join arkusze as AR using(arkusz)
                      where id_testu in (", paste0(rep("?", length(idtestow)), collapse=","), ") and ewd = TRUE")

  tryCatch({
    ret =  sqlExecute(P, zapytanie, fetch = TRUE, stringsAsFactors = FALSE, data=as.list(idtestow))
    odbcClose(P)
  },
  error=function(e) {
    odbcClose(P)
    stop(e)
  }
  ) 
  
  if(nrow(ret) > 1 ){
    stop("Liczba egzaminów wiêksza od 1. Czy na pewno podano poprawne id testów?")
  }
  
  P = odbcConnect(zrodloODBC)
  zapytanie = paste0( "select OB.id_obserwacji, TOB.klasa, TOB.kod_u, 
                      12*(EXTRACT(YEAR FROM T.ostatni_egz) - EXTRACT(YEAR FROM OB.data_ur)) + (EXTRACT(MONTH FROM T.ostatni_egz) - EXTRACT(MONTH FROM OB.data_ur)) as wiek
                      from obserwacje as OB
                      join testy_obserwacje as TOB using(id_obserwacji)
                      join (select 
                              id_testu,
                              ( select  max(AR.data_egzaminu) as ostatni_egz
                                from testy as T
                                join arkusze as AR using(arkusz) 
                                where id_testu in (", paste0(rep("?", length(idtestow)), collapse=","), ") and ewd = TRUE
                              )
                            from testy
                            where ewd = TRUE and id_testu in (", paste0(rep("?", length(idtestow)), collapse=","), ")) as T  using(id_testu)")
  
  tryCatch({
    ret =  sqlExecute(P, zapytanie, fetch = TRUE,  data=c(as.list(idtestow),as.list(idtestow)))
    odbcClose(P)
  },
  error=function(e) {
    odbcClose(P)
    stop(e)
  }
  ) 
  
  if(length(ret$id_obserwacji) != length(unique(ret$id_obserwacji))){
    stop("Przyjanmniej jedna id_obserwacji wystêpuje wiêcej ni¿ jeden raz.")
  }
  
  return(ret)
}
