test_that("top_pays renvoie le bon nombre de lignes", {
  resultat <- top_pays(df_test, "gdp", n = 2)
  expect_equal(nrow(resultat), 2)
})

test_that("top_pays trie en ordre décroissant par défaut", {
  resultat <- top_pays(df_test, "gdp", n = 4)
  expect_true(resultat$gdp[1] >= resultat$gdp[2])
})

test_that("top_pays trie en ordre croissant quand demandé", {
  resultat <- top_pays(df_test, "life_expectancy", n = 4, decroissant = FALSE)
  expect_true(resultat$life_expectancy[1] <= resultat$life_expectancy[2])
})

test_that("top_pays refuse une colonne inexistante", {
  expect_error(top_pays(df_test, "colonne_bidon"))
})

test_that("top_pays refuse une colonne non numérique", {
  expect_error(top_pays(df_test, "country"))
})

test_that("top_pays refuse n négatif", {
  expect_error(top_pays(df_test, "gdp", n = -1))
})



test_that("resume_par_devise produit une ligne par devise", {
  resultat <- resume_par_devise(df_test)
  expect_equal(nrow(resultat), 3)  # EUR, JPY, XOF
  expect_true("EUR" %in% resultat$currency_code)
})

test_that("resume_par_devise agrège correctement la population EUR", {
  resultat <- resume_par_devise(df_test)
  ligne_eur <- resultat[resultat$currency_code == "EUR", ]
  expect_equal(ligne_eur$nb_pays, 2)
  expect_equal(ligne_eur$population_totale, 67390000 + 83240000)
})

test_that("resume_par_devise échoue si une colonne manque", {
  df_incomplet <- df_test[, c("country", "currency_code")]
  expect_error(resume_par_devise(df_incomplet))
})

