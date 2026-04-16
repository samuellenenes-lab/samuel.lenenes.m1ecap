#' Résumer les indicateurs clés par devise
#'
#' Pour chaque code devise, calcule le nombre de pays, la population totale,
#' le PIB total et l'espérance de vie moyenne.
#'
#' @param donnees Un data.frame contenant les colonnes \code{currency_code},
#'   \code{population}, \code{gdp} et \code{life_expectancy}.
#' @return Un data.frame agrégé avec une ligne par devise.
#' @export
#' @examples
#' head(resume_par_devise(world_data))
resume_par_devise <- function(donnees) {
  colonnes_requises <- c("currency_code", "population", "gdp", "life_expectancy")
  if (!all(colonnes_requises %in% names(donnees))) {
    stop("Colonnes manquantes : ",
         paste(setdiff(colonnes_requises, names(donnees)), collapse = ", "))
  }
  devises <- unique(donnees$currency_code)
  devises <- devises[!is.na(devises)]

  resultat <- do.call(rbind, lapply(devises, function(d) {
    subset_d <- donnees[donnees$currency_code == d & !is.na(donnees$currency_code), ]
    data.frame(
      currency_code    = d,
      nb_pays          = nrow(subset_d),
      population_totale = sum(subset_d$population, na.rm = TRUE),
      pib_total        = sum(subset_d$gdp, na.rm = TRUE),
      esperance_vie_moy = mean(subset_d$life_expectancy, na.rm = TRUE)
    )
  }))
  resultat[order(-resultat$population_totale), ]
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
