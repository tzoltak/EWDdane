#' @title Pobieranie danych o szkolach.
#' @description
#' Funkcja pobiera z bazy dane o szkołach - o ich typie i specyfice, nazwie, adresowe
#' i o lokalizacji.
#' @param lata wektor liczb całkowitych - lata, których mają dotyczyć dane (dla każdej
#' szkoły zwrócone zostaną tylko najświeższe dane w ramach tego okresu)
#' @param typySzkol opcjonalny wektor tekstowy z typami szkół, które mają zostać zwrócone
#' (lub NULL - zwraca informacje o wszystkich szkołach)
#' @param idOke wartość logiczna (domyślnie FALSE) - czy dołączać kody OKE szkół?
#' @param daneAdresowe wartość logiczna (domyślnie FALSE) - czy dołączać nazwę i dane
#' adresowe?
#' @return data frame
#' @import dplyr
#' @import ZPD
#' @export
pobierz_dane_szkol = function(lata, typySzkol = NULL, idOke = FALSE,
                              daneAdresowe = FALSE) {
  stopifnot(is.numeric(lata)        , length(lata) > 0,
            is.character(typySzkol) | is.null(typySzkol),
            is.logical(idOke)       , length(idOke) == 1,
            is.logical(daneAdresowe), length(daneAdresowe) == 1
  )
  stopifnot(idOke %in% c(TRUE, FALSE),
            daneAdresowe %in% c(TRUE, FALSE))

  if (length(lata) == 1) lata = rep(lata, 2)  # brzydkie, ale za to 3 wiersze dalej zadziała
  if (length(typySzkol) == 1) typySzkol = rep(typySzkol, 2)  # brzydkie, ale za to 4 wiersze dalej zadziała
  src = polacz()
  on.exit(rozlacz(src))
  szkoly = pobierz_szkoly(src)
  szkoly = filter_(szkoly, ~ rok %in% lata)
  szkoly = select_(szkoly, ~ -wojewodztwo_szkoly, ~ -powiat_szkoly, ~ -gmina_szkoly)
  if (!is.null(typySzkol)) szkoly = filter_(szkoly, ~ typ_szkoly %in% typySzkol)
  if (!idOke) szkoly = select_(szkoly, ~ -id_szkoly_oke)
  if (!daneAdresowe) szkoly = select_(szkoly, ~ -nazwa_szkoly, ~ -adres, ~ -miejscowosc,
                                      ~ -pna, ~ -poczta, ~ -wielkosc_miejscowosci,
                                      ~ -teryt_szkoly, ~ -rodzaj_gminy)
  szkoly = collect(szkoly)
  szkoly = group_by_(szkoly, ~ id_szkoly)
  szkoly = mutate_(szkoly, .dots=list(max_rok = "max(rok)"))
  szkoly = filter_(szkoly, ~ rok == max_rok)
  szkoly = select_(szkoly, ~ -max_rok)
  szkoly = as.data.frame(szkoly)

  typyWWynikach = typySzkol %in% szkoly$typ_szkoly
  if (any(!typyWWynikach)) warning("Nie znaleziono żadnych szkół typu/ów: ",
                                   paste0(typySzkol[!typyWWynikach], collapse=", "), ".")

  attributes(szkoly)$lata = lata
  return(szkoly)
}
