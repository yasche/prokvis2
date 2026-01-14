#' Extract the kinome data frame from a given species from the `kinome_data` object
#'
#' @description Extract the kinome data frame of a given species from the `kinome_data object` by their two-letter code.
#'
#' @param kinome_data A `kinome_data` list
#' @param species_selection The two-letter code of the species
#'
#' @return A tibble containing the kinome data frame of a given species
#'
#' @noRd
extract_kinome_df <- function(kinome_data, species_selection) {
  kinome_data[[species_selection]]$kinome
}
