#' species_selection_choices
#'
#' @description Create the species selection choices from the kinome_data object
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
species_selection_choices <- function(kinome_data) {
  spec_names <- unlist(map(kinome_data, extract_names))

  list(
    unname(spec_names),
    names(spec_names)
  )
}


extract_names <- function(kinome) {
  spec_name <- kinome$name
  spec_sc_name <- kinome$scientific_name

  if (is.null(spec_name)) {
    return(spec_sc_name)
  } else {
    paste0(spec_sc_name, " (", spec_name, ")")
  }
}
