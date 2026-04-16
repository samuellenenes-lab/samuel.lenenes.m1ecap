#' Créer un graphique des 15 pays avec le plus grand PIB
#'
#' Génère un diagramme en barres horizontal montrant les 15 pays
#' ayant le PIB nominal le plus élevé. Utilise ggplot2 pour la visualisation.
#'
#' @param donnees Un data.frame contenant les colonnes \code{country} et \code{gdp}.
#' @param titre Titre du graphique (par défaut "Top 15 des pays par PIB").
#' @return Un objet ggplot2.
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
