#' Filtrer les pays par code devise
#'
#' Renvoie les pays utilisant une devise donnée. Si \code{devise} est
#' \code{NULL}, renvoie l'ensemble du dataset sans filtrage.
#'
#' @param donnees Un data.frame contenant une colonne \code{currency_code}.
#' @param devise Un code devise ISO (ex. \code{"EUR"}, \code{"USD"}).
#'   Si \code{NULL}, aucun filtrage n'est appliqué.
#' @return Un data.frame filtré.
#' @export
#' @examples
#' filtrer_par_devise(world_data, "EUR")
#' filtrer_par_devise(world_data, NULL)
filtrer_par_devise <- function(donnees, devise) {
  if (!is.data.frame(donnees)) {
    stop("L'argument 'donnees' doit etre un data.frame.")
  }
  if (is.null(devise)) {
    return(donnees)
  }
  donnees[donnees$currency_code == devise, ]
}


#' Filtrer les pays par langue officielle
#'
#' @param donnees Un data.frame contenant une colonne \code{official_language}.
#' @param langue Nom de la langue (ex. \code{"French"}, \code{"Arabic"}).
#'   Si \code{NULL}, renvoie tout le dataset.
#' @return Un data.frame filtré.
#' @export
#' @examples
#' filtrer_par_langue(world_data, "French")
filtrer_par_langue <- function(donnees, langue) {
  if (!is.data.frame(donnees)) {
    stop("L'argument 'donnees' doit etre un data.frame.")
  }
  if (is.null(langue)) {
    return(donnees)
  }
  donnees[donnees$official_language == langue, ]
}
