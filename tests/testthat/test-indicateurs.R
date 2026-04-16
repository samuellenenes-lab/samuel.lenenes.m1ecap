test_that("calculer_pib_par_hab ajoute la bonne colonne", {
  df <- data.frame(gdp = c(1000, 2000), population = c(10, 20))
  resultat <- calculer_pib_par_hab(df)
  expect_true("gdp_per_capita" %in% names(resultat))
  expect_equal(resultat$gdp_per_capita, c(100, 100))
})

test_that("calculer_pib_par_hab gère les NA", {
  df <- data.frame(gdp = c(1000, NA), population = c(10, 50))
  resultat <- calculer_pib_par_hab(df)
  expect_true(is.na(resultat$gdp_per_capita[2]))
})

test_that("calculer_pib_par_hab échoue si les colonnes manquent", {
  df <- data.frame(pays = "France", hab = 67000000)
  expect_error(calculer_pib_par_hab(df), "gdp")
})

test_that("comparer_pays renvoie exactement 2 lignes", {
  resultat <- comparer_pays(df_test, "France", "Japan")
  expect_equal(nrow(resultat), 2)
})

test_that("comparer_pays renvoie les bonnes colonnes", {
  resultat <- comparer_pays(df_test, "France", "Japan",
                            indicateurs = c("gdp", "population"))
  expect_equal(names(resultat), c("country", "gdp", "population"))
})

test_that("comparer_pays refuse un pays inconnu", {
  expect_error(comparer_pays(df_test, "France", "Narnia"))
})

test_that("comparer_pays refuse un indicateur inexistant", {
  expect_error(comparer_pays(df_test, "France", "Japan",
                             indicateurs = c("gdp", "bonheur")))
})

test_that("moyenne_esperance_par_devise calcule les moyennes correctement", {
  resultat <- moyenne_esperance_par_devise(df_test)
  expect_equal(nrow(resultat), 3)  # EUR, JPY, XOF
  ligne_eur <- resultat[resultat$currency_code == "EUR", ]
  esp_attente <- mean(c(82.5, 81.3))  # France et Germany
  expect_equal(ligne_eur$mean_life_expectancy, esp_attente)
})

test_that("moyenne_esperance_par_devise compte les pays correctement", {
  resultat <- moyenne_esperance_par_devise(df_test)
  ligne_eur <- resultat[resultat$currency_code == "EUR", ]
  expect_equal(ligne_eur$nb_pays, 2)
  ligne_jpy <- resultat[resultat$currency_code == "JPY", ]
  expect_equal(ligne_jpy$nb_pays, 1)
})

test_that("moyenne_esperance_par_devise calcule l'écart-type", {
  resultat <- moyenne_esperance_par_devise(df_test)
  ligne_eur <- resultat[resultat$currency_code == "EUR", ]
  expect_true(!is.na(ligne_eur$sd_life_expectancy))
  expect_true(is.numeric(ligne_eur$sd_life_expectancy))
})

test_that("moyenne_esperance_par_devise gère les NA", {
  df_avec_na <- df_test
  df_avec_na$life_expectancy[2] <- NA
  resultat <- moyenne_esperance_par_devise(df_avec_na)
  expect_true(nrow(resultat) > 0)
  expect_true(all(!is.na(resultat$mean_life_expectancy)))
})

test_that("moyenne_esperance_par_devise échoue si colonnes manquent", {
  df_incomplet <- df_test[, c("country", "currency_code")]
  expect_error(moyenne_esperance_par_devise(df_incomplet))
})

