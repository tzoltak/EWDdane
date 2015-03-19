context('pobierz_dane_szkol') # cokolwiek - to się po prostu wyświetla na ekranie podczas uruchamiania testów jako taka wskazówka, co się właśnie testuje

# testy grupujemy w nazwane grupy, ale znów od strony technicznej wszystko jedno, jak to zrobimy
# mogą być wszystkie w jednym test_that() równie dobrze jak każdy w oddzielnym
test_that('pobierz_dane_szkol działa', {
	szkoly = pobierz_dane_szkol(2010, 'T')
	expect_is(szkoly, 'data.frame')
	expect_equal(all(szkoly$typ_szkoly %in% 'T'), TRUE)
	expect_equal(all(!is.na(szkoly$id_szkoly)), TRUE)
})
