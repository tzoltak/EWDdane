context('pobierz_wartosci_wskaznikow_ewd') # cokolwiek - to się po prostu wyświetla na ekranie podczas uruchamiania testów jako taka wskazówka, co się właśnie testuje

# testy grupujemy w nazwane grupy, ale znów od strony technicznej wszystko jedno, jak to zrobimy
# mogą być wszystkie w jednym test_that() równie dobrze jak każdy w oddzielnym
test_that('pobierz_wartosci_wskaznikow_ewd działa', {
	dane = pobierz_wartosci_wskaznikow_ewd('T', 2013)
	expect_is(dane, 'data.frame')
	expect_equal(all(dane$typ_szkoly %in% 'T'), TRUE)
	#expect_equal(all(!is.na(dane$nazwa)), TRUE)
})
