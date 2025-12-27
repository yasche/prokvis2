#' species_selection_choices
#'
#' @description Create the species selection choices from the kinome_data object
#'
#' @param kinome_data A kinome_data list object.
#'
#' @return A named character vector that contains the species to be used in a selectInput.
#'
#' @noRd
species_selection_choices <- function(kinome_data) {
  spec_list <- unlist(purrr::map(kinome_data, extract_names))

  spec_names <- names(spec_list)
  names(spec_names) <- unname(spec_list)

  spec_names
}


#' Helper function for joining species name and scientific name
#'
#' @param kinome A kinome from the kinome_data object.
#'
#' @returns A string in the following form: `scientific_name (name)` or `scientific_name` in case no `name` is available.
#'
#' @noRd
extract_names <- function(kinome) {
  spec_name <- kinome$name
  spec_sc_name <- kinome$scientific_name

  if (is.null(spec_name)) {
    return(spec_sc_name)
  } else {
    paste0(spec_sc_name, " (", spec_name, ")")
  }
}
