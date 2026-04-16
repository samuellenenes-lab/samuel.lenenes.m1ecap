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
#' @importFrom dplyr group_by summarise n
#' @importFrom stats sd
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

#' Top N des pays selon un indicateur
#'
#' @param donnees Un data.frame.
#' @param indicateur Nom de la colonne numérique à classer (chaîne de
#'   caractères).
#' @param n Nombre de pays à renvoyer (défaut : 10).
#' @param decroissant Logique. Si \code{TRUE} (défaut), renvoie les plus
#'   grandes valeurs.
#' @return Un data.frame trié contenant \code{n} lignes.
#' @importFrom utils head
#' @export
#' @examples
#' top_pays(world_data, "gdp", n = 5)
#' top_pays(world_data, "life_expectancy", n = 10)
#' top_pays(world_data, "infant_mortality", n = 5, decroissant = FALSE)
top_pays <- function(donnees, indicateur, n = 10, decroissant = TRUE) {
  if (!indicateur %in% names(donnees)) {
    stop("La colonne '", indicateur, "' n'existe pas dans le data.frame.")
  }
  if (!is.numeric(donnees[[indicateur]])) {
    stop("La colonne '", indicateur, "' n'est pas numerique.")
  }
  if (n < 1) {
    stop("'n' doit etre un entier positif.")
  }
  donnees_triees <- donnees[order(donnees[[indicateur]],
                                  decreasing = decroissant), ]
  donnees_sans_na <- donnees_triees[!is.na(donnees_triees[[indicateur]]), ]
  head(donnees_sans_na, n)
}

#' Créer un graphique des 15 pays avec le plus grand PIB
#'
#' Génère un diagramme en barres horizontal montrant les 15 pays
#' ayant le PIB nominal le plus élevé. Utilise ggplot2 pour la visualisation.
#'
#' @param donnees Un data.frame contenant les colonnes \code{country} et \code{gdp}.
#' @param titre Titre du graphique (par défaut "Top 15 des pays par PIB").
#' @return Un objet ggplot2.
#' @importFrom ggplot2 ggplot aes geom_col scale_fill_viridis_c coord_flip labs theme_minimal theme element_text
#' @importFrom stats reorder
#' @export
#' @examples
#' visualiser_top_pib(world_data)
#' visualiser_top_pib(world_data, titre = "Économies mondiales")
visualiser_top_pib <- function(donnees, titre = "Top 15 des pays par PIB") {
  if (!is.data.frame(donnees)) {
    stop("L'argument 'donnees' doit etre un data.frame.")
  }
  if (!("country" %in% names(donnees)) || !("gdp" %in% names(donnees))) {
    stop("Les colonnes 'country' et 'gdp' sont requises.")
  }

  # Récupérer le top 15 et ordonner
  donnees_ordered <- donnees[order(donnees$gdp, decreasing = TRUE), ]
  top_15 <- donnees_ordered[1:15, ]
  top_15 <- top_15[!is.na(top_15$gdp), ]

  if (nrow(top_15) == 0) {
    stop("Aucune donnée valide pour créer le graphique.")
  }

  # Créer le facteur avec les niveaux ordonnés
  top_15$country <- factor(top_15$country, levels = top_15$country)

  # Créer le graphique
  ggplot2::ggplot(top_15, ggplot2::aes(x = stats::reorder(country, gdp), y = gdp / 1e12, fill = gdp)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::scale_fill_viridis_c(direction = -1) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = titre,
      x = "Pays",
      y = "PIB (en milliers de milliards USD)",
      caption = "Source: world_data 2023"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5)
    )
}
