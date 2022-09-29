#' @title Aktualizacja bazy danych szkol
#' @description
#' Funkcja weryfikuje poprawność bazy szkół, przygotowanej w pliku csv, po
#' przeprowadzeniu jej ręcznej aktualizacji.
#' @param bazaZakt nazwa (ścieżka do) pliku w formacie csv
#' zawierającego zaktualizowaną bazę szkół lub data frame z taką wczytaną bazą
#' @param bazaZrzut opcjonalnie nazwa (ścieżka do) pliku w formacie csv
#' zawierającego zrzut bazy szkół, na który nanoszona była aktualizacja lub data
#' frame z taką wczytaną bazą
#' @details Jeśli parametr \code{bazaZrzut} nie zostanie podany, funkcja jedynie
#' zweryfikuje poprawność podanej bazy. Jeśli zostanie on podany, o ile nie
#' zostaną wykryte błędy polegające na duplikacji identyfikatorów OKE, funkcja
#' zbada również zakres zmian w stosunku do zrzutu bazy.
#' @return lista data frame'ów
#' @importFrom utils read.csv2
#' @export
weryfikuj_baze_szkol = function(bazaZakt, bazaZrzut = NULL) {
  stopifnot(is.data.frame(bazaZakt) | is.character(bazaZakt))
  if (is.character(bazaZakt)) {
    stopifnot(length(bazaZakt) == 1, file.exists(bazaZakt))
    bazaZakt = read.csv2(bazaZakt, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    stopifnot(ncol(bazaZakt) > 1)
  } else {
    maska = unlist(lapply(bazaZakt, is.factor))
    bazaZakt[, maska] = lapply(bazaZakt[, maska],
                               function(x) {return(levels(x)[x])})
  }
  if (!is.null(bazaZrzut)) {
    stopifnot(is.data.frame(bazaZrzut) | is.character(bazaZrzut))
  }
  if (is.character(bazaZrzut)) {
    stopifnot(length(bazaZrzut) == 1, file.exists(bazaZrzut))
    bazaZrzut = read.csv2(bazaZrzut, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
    stopifnot(ncol(bazaZrzut) > 1)
  } else {
    maska = unlist(lapply(bazaZrzut, is.factor))
    bazaZrzut[, maska] = lapply(bazaZrzut[, maska],
                                function(x) {return(levels(x)[x])})
  }

  problemy = vector(mode = "list", length = 4)
  names(problemy) = c("duplikaty", "adresWNazwie", "miejscowoscJakoUlica",
                      "niepoprawnyPna")
  # szukanie duplikatów
  maskaKodyOke = grepl("^id_szkoly_oke_|^kod_(g|lo|t)_", names(bazaZakt))
  message("Liczba duplikatów id_szkoly_oke_rrrr dla lat:")
  for (i in names(bazaZakt)[maskaKodyOke]) {
    bazaZakt[is.na(bazaZakt[, i]), i] = ""
    message("   ", gsub("[^[:digit:]]", "", i), ": ",
        sum(duplicated(bazaZakt[bazaZakt[, i] != "", i])),
        " z ", sum(bazaZakt[, i] != ""))
  }
  wszystkieKody = unlist(apply(bazaZakt[, maskaKodyOke], 1,
                               function(x) {return(unique(x[x != ""]))}))
  powtKody = table(wszystkieKody)[table(wszystkieKody) > 1]
  message("Liczba powtórzeń wykorzystania id_szkoly_oke między latami: ",
          length(powtKody))

  message(paste0(paste0("  ", names(powtKody)), collapse = "\n"))
  maskaWiersze = apply(bazaZakt[, maskaKodyOke], 1,
                       function(x, powtKody) {return(any(x %in% powtKody))},
                       powtKody = names(powtKody))
  maskaKolumny = maskaKodyOke | grepl("^id_|^nazwa", names(bazaZakt))
  problemy$duplikaty = bazaZakt[maskaWiersze, maskaKolumny, drop = FALSE]

  # trochę sprawdzania nazw i adresów
  message("Szkoły z adresami w nazwie (szukam kodów pocztowych):")
  maskaNazwa = grepl("^nazwa", names(bazaZakt))
  problemy$adresWNazwie =
    bazaZakt[apply(bazaZakt[, maskaNazwa, drop = FALSE], 1,
                   function(x) {
                     return(any(grepl("[^[:digit:]][[:digit:]]{2}[-][[:digit:]]{3}[^[:digit:]]", x)))
                   }),
             grepl("^id_szkoly$|^id_(g|lo|t)|^nazwa",
                                         names(bazaZakt))]
  if (nrow(problemy$adresWNazwie) > 0) {
    print(problemy$adresWNazwie, row.names = FALSE)
  } else {
    cat("  Nie wystąpiły.\n")
  }

  message("Szkoły z nazwą miejscowości jako ulicą:")
  problemy$miejscowoscJakoUlica = bazaZakt[!grepl("^[[:digit:]]+[[:lower:][:upper:]]?$",
                                                  bazaZakt$adres)
                                           & grepl("^[ [:digit:]]+$", mapply(
                                             function(x, y) {
                                               return(sub(y, "", x))
                                             },
                                             bazaZakt$adres, bazaZakt$miejscowosc
                                           )),
                                           grepl("^id_szkoly$|^id_(g|lo|t)|^adres$|^miejscowosc$",
                                                 names(bazaZakt))]
  if (nrow(problemy$miejscowoscJakoUlica) > 0) {
    print(problemy$miejscowoscJakoUlica, row.names = FALSE)
  } else {
    cat("  Nie wystąpiły.\n")
  }

  message("Szkoły z niepoprawnymi PNA (nie przystają do wzorca dd-ddd):")
  problemy$niepoprawnyPna = bazaZakt[!grepl("^[[:digit:]]{2}[-][[:digit:]]{3}$|^$",
                                            bazaZakt$pna),
                                     grepl("^id_szkoly$|^id_(g|lo|t)|^pna$|^poczta$",
                                           names(bazaZakt))]
  if (nrow(problemy$niepoprawnyPna) > 0) {
    print(problemy$niepoprawnyPna, row.names = FALSE)
  } else {
    cat("  Nie wystąpiły.\n")
  }

  if (nrow(problemy$duplikaty) == 0 & !is.null(bazaZrzut)) {
    zmiany = vector(mode = "list", length = 2)
    names(zmiany) = c("zmianyId", "zmianyKodowOke")
    # sprawdzamy, co się zmieniło w stosunku do zrzutu
    maskaIdSzkolyZakt = grepl("^id_szkoly(|_strona)$", names(bazaZakt))
    maskaIdSzkolyZrzut = grepl("^id_szkoly(|_strona)$", names(bazaZrzut))
    temp = bazaZakt[!is.na(bazaZakt[, maskaIdSzkolyZakt]), ]
    maskaUsuniete = !(bazaZrzut[, maskaIdSzkolyZrzut] %in% temp[, maskaIdSzkolyZakt])
    usuniete = bazaZrzut[maskaUsuniete, maskaIdSzkolyZrzut]
    zmiany$zmianyId   = as.data.frame(matrix(0, ncol = 3, nrow = 0))
    names(zmiany$zmianyId) = c("id_szkoly było", "id_szkoly ma być", "rok")
    maskaKodyOkeZrzut = grepl("^id_szkoly_oke_|^kod_(g|lo|t)_", names(bazaZrzut))
    for (i in usuniete) {
      for (j in names(bazaZrzut)[maskaKodyOkeZrzut]) {
        kodOkeTemp = bazaZrzut[bazaZrzut[, maskaIdSzkolyZrzut] == i, j]
        if (kodOkeTemp != "") {
          if (kodOkeTemp %in% bazaZakt[, j]) {
            zmiany$zmianyId[nrow(zmiany$zmianyId) + 1, ] = c(
              i,
              bazaZakt[kodOkeTemp == bazaZakt[, j], maskaIdSzkolyZakt],
              as.numeric(gsub("[^[:digit:]]", "", j)))
          } else {
            zmiany$zmianyId[nrow(zmiany$zmianyId) + 1, ] = c(i, NA,
                                           as.numeric(gsub("[^[:digit:]]", "", j)))
          }
        }
      }
    }
    message("Do bazy dodano nowych szkół: ", sum(is.na(bazaZakt[, maskaIdSzkolyZakt])),
        "\nZ bazy usunięto szkół: ", length(usuniete),
        "\n\nZmiany w bazie szkół:")
    print(zmiany$zmianyId, row.names = FALSE)

    # i jeszcze śledzenie ew. zmian kodów OKE w stosunku do zrzutu
    maskaZmienne = intersect(names(bazaZrzut), names(bazaZakt))
    maskaZmienne = grep("^id_szkoly(|_strona)$|^id_szkoly_oke_|^kod_(g|lo|t)_",
                        maskaZmienne, value = TRUE)
    polaczone = merge(
      bazaZrzut[, maskaZmienne],
      bazaZakt[, maskaZmienne],
      by = names(bazaZakt)[maskaIdSzkolyZakt],
      suffixes = c("", "_zakt")
    )
    polaczone[is.na(polaczone)] = ""
    maskaZmienne = grep("^id_szkoly_oke_|^kod_(g|lo|t)_", maskaZmienne,
                        value = TRUE)
    temp = polaczone[, maskaZmienne] != polaczone[, paste0(maskaZmienne, "_zakt")]
    maska = apply(temp, 1, any)
    polaczone = polaczone[maska %in% TRUE, ]
    if (any(maska)) {
      maska = polaczone[, maskaZmienne] == polaczone[, paste0(maskaZmienne, "_zakt")]
      polaczone[, maskaZmienne][maska] = ""
      polaczone[, paste0(maskaZmienne, "_zakt")][maska] = ""
      temp = polaczone[, maskaZmienne] != polaczone[, paste0(maskaZmienne, "_zakt")]
      maska = apply(temp, 2, any)
      maskaZmienne = c(
        names(polaczone)[grep("^id_szkoly(|_strona)$", names(polaczone))],
        maskaZmienne[maska], paste0(maskaZmienne, "_zakt")[maska]
      )
      zmiany$zmianyKodowOke = polaczone[, maskaZmienne]
      message("Zmiany id OKE szkół w latach wcześniejszych:\n")
      print(zmiany$zmianyKodowOke, row.names = FALSE)
    } else {
      zmiany$zmianyKodowOke = NULL
      message("Nie znaleziono żadnych zmian id OKE szkół w latach wcześniejszych.\n")
    }
    return(zmiany)
  } else if (nrow(problemy$duplikaty) > 0) {
    warning("Wykryto problemy z duplikatami kodów OKE szkół.", immediate. = TRUE)
  }
  invisible(problemy)
}
