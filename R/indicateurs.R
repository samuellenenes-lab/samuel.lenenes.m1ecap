#' Calculer le PIB par habitant
#'
#' Ajoute une colonne \code{gdp_per_capita} au data.frame.
#'
#' @param donnees Un data.frame avec les colonnes \code{gdp} et
#'   \code{population}.
#' @return Le data.frame enrichi d'une colonne \code{gdp_per_capita}.
#' @export
#' @examples
#' head(calculer_pib_par_hab(world_data))
calculer_pib_par_hab <- function(donnees) {
  if (!all(c("gdp", "population") %in% names(donnees))) {
    stop("Le data.frame doit contenir les colonnes 'gdp' et 'population'.")
  }
  donnees$gdp_per_capita <- donnees$gdp / donnees$population
  donnees
}


#' Comparer deux pays sur une sélection d'indicateurs
#'
#' @param donnees Un data.frame contenant une colonne \code{country}.
#' @param pays_a Nom du premier pays.
#' @param pays_b Nom du second pays.
#' @param indicateurs Vecteur de noms de colonnes à comparer. Par défaut :
#'   PIB, population, espérance de vie, émissions CO2.
#' @return Un data.frame avec les deux pays en lignes et les indicateurs en
#'   colonnes.
#' @export
#' @examples
#' comparer_pays(world_data, "France", "Germany")
comparer_pays <- function(donnees, pays_a, pays_b,
                          indicateurs = c("gdp", "population",
                                          "life_expectancy", "co2_emissions")) {
  if (!pays_a %in% donnees$country) {
    stop("Pays introuvable : ", pays_a)
  }
  if (!pays_b %in% donnees$country) {
    stop("Pays introuvable : ", pays_b)
  }
  manquants <- setdiff(indicateurs, names(donnees))
  if (length(manquants) > 0) {
    stop("Colonnes manquantes : ", paste(manquants, collapse = ", "))
  }
  selection <- donnees[donnees$country %in% c(pays_a, pays_b),
                       c("country", indicateurs)]
  selection
}

#' Calculer la moyenne de l'espérance de vie par devise
#'
#' Agrège l'espérance de vie par code devise et renvoie la moyenne,
#' le nombre de pays et l'écart-type pour chaque devise.
#'
#' @param donnees Un data.frame contenant les colonnes \code{currency_code}
#'   et \code{life_expectancy}.
#' @return Un data.frame avec colonnes : \code{currency_code},
#'   \code{mean_life_expectancy}, \code{sd_life_expectancy}, \code{nb_pays}.
#' @export
#' @examples
#' moyenne_esperance_par_devise(world_data)
#' head(moyenne_esperance_par_devise(world_data))
moyenne_esperance_par_devise <- function(donnees) {
  if (!is.data.frame(donnees)) {
    stop("L'argument 'donnees' doit etre un data.frame.")
  }
  if (!("currency_code" %in% names(donnees)) || !("life_expectancy" %in% names(donnees))) {
    stop("Les colonnes 'currency_code' et 'life_expectancy' sont requises.")
  }

  # Supprimer les lignes avec valeurs manquantes
  donnees_clean <- donnees[!is.na(donnees$life_expectancy), ]

  if (nrow(donnees_clean) == 0) {
    stop("Aucune valeur valide pour life_expectancy.")
  }

  # Agréger par devise avec dplyr
  donnees_clean |>
    dplyr::group_by(currency_code) |>
    dplyr::summarise(
      mean_life_expectancy = mean(life_expectancy, na.rm = TRUE),
      sd_life_expectancy = sd(life_expectancy, na.rm = TRUE),
      nb_pays = dplyr::n(),
      .groups = "drop"
    ) |>
    as.data.frame()
}

