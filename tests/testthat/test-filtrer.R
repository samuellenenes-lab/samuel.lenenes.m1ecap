# --- Résultat attendu ---
test_that("filtrer_par_devise renvoie les bons pays pour EUR", {
  resultat <- filtrer_par_devise(df_test, "EUR")
  expect_equal(nrow(resultat), 2)
  expect_true(all(resultat$currency_code == "EUR"))
  expect_true("France" %in% resultat$country)
  expect_true("Germany" %in% resultat$country)
})

# --- Devise absente du dataset ---
test_that("filtrer_par_devise renvoie 0 lignes si la devise n'existe pas", {
  resultat <- filtrer_par_devise(df_test, "BTC")
  expect_equal(nrow(resultat), 0)
})

# --- Cas NULL : aucun filtrage ---
test_that("filtrer_par_devise avec NULL renvoie tout le dataset", {
  resultat <- filtrer_par_devise(df_test, NULL)
  expect_equal(nrow(resultat), nrow(df_test))
})

# --- Erreur attendue : mauvais type d'entrée ---
test_that("filtrer_par_devise refuse un vecteur au lieu d'un data.frame", {
  expect_error(filtrer_par_devise(c(1, 2, 3), "EUR"))
})

test_that("filtrer_par_devise refuse une chaîne au lieu d'un data.frame", {
  expect_error(filtrer_par_devise("pas un df", "EUR"))
})


test_that("filtrer_par_langue isole les pays francophones", {
  df <- data.frame(
    country           = c("France", "Japan", "Senegal"),
    official_language  = c("French", "Japanese", "French"),
    stringsAsFactors   = FALSE
  )
  resultat <- filtrer_par_langue(df, "French")
  expect_equal(nrow(resultat), 2)
  expect_true(all(resultat$official_language == "French"))
})

test_that("filtrer_par_langue avec NULL renvoie tout", {
  df <- data.frame(
    country           = c("France", "Japan"),
    official_language  = c("French", "Japanese"),
    stringsAsFactors   = FALSE
  )
  expect_equal(nrow(filtrer_par_langue(df, NULL)), 2)
})
