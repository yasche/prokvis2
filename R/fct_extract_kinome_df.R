#' extract_kinome_df
#'
#' @description A fct function
#'
#' @param kinome_data A `kinome_data` list
#' @param species_selection The two-letter code of the species
#'
#' @return A reactive containing the kinome data frame.
#'
#' @noRd
extract_kinome_df <- function(kinome_data, species_selection) {
  kinome_data[[species_selection]]$kinome
}
