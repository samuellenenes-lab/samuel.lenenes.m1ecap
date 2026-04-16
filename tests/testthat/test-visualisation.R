test_that("visualiser_top_pib renvoie un objet ggplot", {
  resultat <- visualiser_top_pib(df_test)
  expect_true(ggplot2::is.ggplot(resultat))
})

test_that("visualiser_top_pib crée un graphique avec le bon titre par défaut", {
  resultat <- visualiser_top_pib(df_test)
  expect_equal(resultat$labels$title, "Top 15 des pays par PIB")
})

test_that("visualiser_top_pib accepte un titre personnalisé", {
  titre_custom <- "Économies mondiales 2023"
  resultat <- visualiser_top_pib(df_test, titre = titre_custom)
  expect_equal(resultat$labels$title, titre_custom)
})

test_that("visualiser_top_pib refuse un data.frame vide", {
  df_vide <- df_test[FALSE, ]
  expect_error(visualiser_top_pib(df_vide))
})

test_that("visualiser_top_pib refuse si colonne gdp manque", {
  df_sans_gdp <- df_test[, c("country", "population")]
  expect_error(visualiser_top_pib(df_sans_gdp))
})

test_that("visualiser_top_pib refuse si colonne country manque", {
  df_sans_country <- df_test[, c("gdp", "population")]
  expect_error(visualiser_top_pib(df_sans_country))
})

test_that("visualiser_top_pib gère les NA dans gdp", {
  df_avec_na <- df_test
  df_avec_na$gdp[1] <- NA
  resultat <- visualiser_top_pib(df_avec_na)
  expect_true(ggplot2::is.ggplot(resultat))
})
