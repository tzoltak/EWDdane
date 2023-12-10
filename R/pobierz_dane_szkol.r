#' @title Pobieranie danych o szkolach
#' @description
#' Funkcja pobiera z bazy dane o szkołach - o ich typie i specyfice, nazwie,
#' adresowe i o lokalizacji.
#' @param lata wektor liczb całkowitych - lata, których mają dotyczyć dane (dla
#' każdej szkoły zwrócone zostaną tylko najświeższe dane w ramach tego okresu)
#' @param typySzkol opcjonalny wektor tekstowy z typami szkół, które mają zostać
#' zwrócone (lub NULL - zwraca informacje o wszystkich szkołach)
#' @param idOke wartość logiczna (domyślnie FALSE) - czy dołączać kody OKE szkół?
#' @param daneAdresowe wartość logiczna (domyślnie FALSE) - czy dołączać nazwę
#' i dane adresowe?
#' @param src NULL połączenie z bazą danych IBE zwracane przez funkcję
#' \code{\link[ZPD]{polacz}}; pozwala posłużyć się niestandardowymi parametrami
#' połączenia
#' @return data frame
#' @import dplyr
#' @import ZPD
#' @export
pobierz_dane_szkol = function(lata, typySzkol = NULL, idOke = FALSE,
                              daneAdresowe = FALSE, src = NULL) {
  stopifnot(is.numeric(lata)        , length(lata) > 0,
            is.character(typySzkol) | is.null(typySzkol),
            is.logical(idOke)       , length(idOke) == 1,
            is.logical(daneAdresowe), length(daneAdresowe) == 1,
            is.src(src) | is.null(src)
  )
  stopifnot(idOke %in% c(TRUE, FALSE),
            daneAdresowe %in% c(TRUE, FALSE))
  if (is.null(src)) {
    src = ZPD::polacz()
  }

  if (length(lata) == 1) lata = rep(lata, 2)  # brzydkie, ale za to 3 wiersze dalej zadziała
  if (length(typySzkol) == 1) typySzkol = rep(typySzkol, 2)  # brzydkie, ale za to 4 wiersze dalej zadziała
  szkoly = pobierz_szkoly(src) %>%
    filter(.data$rok %in% lata) %>%
    select(-c("wojewodztwo_szkoly", "powiat_szkoly", "gmina_szkoly"))
  if (!is.null(typySzkol)) szkoly = filter(szkoly, .data$typ_szkoly %in% typySzkol)
  if (!idOke) szkoly = select(szkoly, -"id_szkoly_oke")
  if (!daneAdresowe) {
    szkoly = select(szkoly, -c("nazwa_szkoly", "adres", "miejscowosc", "pna",
                               "poczta", "wielkosc_miejscowosci",
                               "teryt_szkoly", "rodzaj_gminy"))
  }
  szkoly = collect(szkoly, n = Inf) %>%
    group_by(.data$id_szkoly) %>%
    filter(.data$rok == max(.data$rok)) %>%
    as.data.frame()

  typyWWynikach = typySzkol %in% szkoly$typ_szkoly
  if (any(!typyWWynikach)) warning("Nie znaleziono żadnych szkół typu/ów: ",
                                   paste0(typySzkol[!typyWWynikach], collapse = ", "), ".")
  attributes(szkoly)$lata = lata
  return(szkoly)
}
