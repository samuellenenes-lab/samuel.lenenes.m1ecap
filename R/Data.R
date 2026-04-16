#' Indicateurs mondiaux 2023
#'
#' Jeu de données décrivant 195 pays à l'aide d'indicateurs
#' socio-économiques, démographiques et environnementaux pour l'année 2023.
#'
#' @format Un data.frame de 195 lignes et 35 colonnes.
#'
#' @section Informations générales:
#' \itemize{
#'   \item \strong{\code{country}} : Nom du pays
#'   \item \strong{\code{abbreviation}} : Code ISO à 2 lettres
#'   \item \strong{\code{currency_code}} : Code ISO de la devise (EUR, USD, ...)
#'   \item \strong{\code{official_language}} : Langue officielle
#' }
#'
#' @section Démographie:
#' \itemize{
#'   \item \strong{\code{population}} : Population totale
#'   \item \strong{\code{life_expectancy}} : Espérance de vie en années
#'   \item \strong{\code{birth_rate}} : Taux de natalité pour 1 000 habitants
#'   \item \strong{\code{infant_mortality}} : Mortalité infantile pour 1 000 naissances
#'   \item \strong{\code{fertility_rate}} : Indice de fécondité
#' }
#'
#' @section Économie:
#' \itemize{
#'   \item \strong{\code{gdp}} : PIB en dollars US
#'   \item \strong{\code{unemployment_rate}} : Taux de chômage en pourcentage
#' }
#'
#' @section Environnement:
#' \itemize{
#'   \item \strong{\code{co2_emissions}} : Émissions de CO2 en tonnes
#'   \item \strong{\code{forested_area_percent}} : Surface forestière en pourcentage
#' }
#'
#' @section Géographie:
#' \itemize{
#'   \item \strong{\code{latitude}} : Latitude du centroïde
#'   \item \strong{\code{longitude}} : Longitude du centroïde
#' }
#'
#' @source
#' Données compilées à partir de sources publiques (2023). \cr
#' \url{https://www.kaggle.com/datasets/nelgiriyewithana/countries-of-the-world-2023}
#'
#' @docType data
#' @keywords datasets
"world_data"
