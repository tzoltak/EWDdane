context('pobierz_dane_kontekstowe') # cokolwiek - to się po prostu wyświetla na ekranie podczas uruchamiania testów jako taka wskazówka, co się właśnie testuje

# testy grupujemy w nazwane grupy, ale znów od strony technicznej wszystko jedno, jak to zrobimy
# mogą być wszystkie w jednym test_that() równie dobrze jak każdy w oddzielnym
test_that('pobierz_dane_kontekstowe działa', {
  src = polacz()
  dane = pobierz_dane_kontekstowe(src, 'sprawdzian', FALSE)
  expect_more_than(nrow(dane), 10^6)
  expect_equal(min(dane$rok), 2002)
  expect_more_than(mean(dane$rok), 2007)
  expect_equal(unique(dane$typ_szkoly), 'SP')
  
  dane = pobierz_dane_kontekstowe(src, 'egzamin gimnazjalny', TRUE)
  expect_more_than(nrow(dane), 10^6)
  expect_equal(min(dane$rok), 2004)
  expect_more_than(mean(dane$rok), 2009)
  expect_equal(unique(dane$typ_szkoly), 'gimn.')

  dane = pobierz_dane_kontekstowe(src, 'matura', TRUE)
  expect_more_than(nrow(dane), 10^6)
  expect_equal(min(dane$rok), 2010)
  expect_more_than(mean(dane$rok), 2011)
  expect_equal(unique(dane$typ_szkoly), c('LO', 'T', 'LP', 'LOU', 'TU'))
})
