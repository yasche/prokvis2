#' Map kinase names to their manning names
#'
#' @description Function to map an identifier, for example a gene name or a UniProt accession, to a Manning kinase name.
#'
#' @param kinome_data A `kinome_data` object
#' @param species_selection The two-letter code of the species (e.g., `hs` for human)
#' @param kinase_names Character value where the kinases to map are separated by a linebreak (i.e., `\n`)
#'
#' @return A Tibble with the columns `Input`, (the suggested) `Manning Name`, `Include` and `Aliases`.
#'
#' @noRd
map_names <- function(kinome_data, species_selection, kinase_names, aliases) {
  input_kinases <- kinase_names %>%
    stringr::str_split("\\n") %>%
    unlist()

  matched_kinases <- purrr::map(input_kinases, grep, aliases$Alias, ignore.case = T)

  names(matched_kinases) <- input_kinases

  purrr::map(matched_kinases, select_rows_int, aliases) %>%
    dplyr::bind_rows(.id = "Input") %>%
    dplyr::transmute(Input = .data$Input,
                     Manning_Name = .data$Gene,
                     Include = TRUE,
                     Aliases = .data$Alias)
}

#' Helper function to select rows in a data frame by index
#'
#' @description Small helper function
#'
#' @param indices A numeric - Which rows to keep.
#' @param df A data frame.
#'
#' @return a subset of `df` which only contains the `indices`.
#'
#' @noRd
select_rows_int <- function(indices, df) {
  df[indices, ]
}

#' Turn the name map tibble into an rhot object
#'
#' @param name_map_df A tibble - created with `map_names()`
#' @param aliases A tibble - the aliases of a species, created with `extract_aliases_df()`
#'
#' @returns An rhot onject.
#'
#' @noRd
name_map_df2rhot <- function(name_map_df, aliases) {
  rhandsontable::rhandsontable(name_map_df, height = 500) %>%
    rhandsontable::hot_table(stretchH = "all") %>%
    rhandsontable::hot_col(1, readOnly = T) %>%
    rhandsontable::hot_col(2, type = "dropdown", source = aliases$Gene, strict = TRUE) %>%
    rhandsontable::hot_col(3, type = "checkbox") %>%
    rhandsontable::hot_col(4, readOnly = T) %>%
    rhandsontable::hot_rows(rowHeights = 25)
}

#' Helper function to extract the alias df from kinome_data list
#'
#' @param kinome_data A `kinome_data` object
#' @param species_selection The two-letter code of the species (e.g., `hs` for human)
#'
#' @return A data frame containing the aliases for a given species.
#'
#' @noRd
extract_aliases_df <- function(kinome_data, species_selection) {
  kinome_data[[species_selection]]$aliases
}

#' Wrapper function for easier inclusion in the server function
#'
#' @description Create an rhot object to use it in the name map tab.
#'
#' @param kinome_data A `kinome_data` object
#' @param species_selection The two-letter code of the species (e.g., `hs` for human)
#' @param kinase_names The names to map.
#'
#' @return The result from `map_names` converted to an rhot.
#'
#' @noRd
name_map_rhot <- function(kinome_data, species_selection, kinase_names) {
  aliases <- extract_aliases_df(kinome_data, species_selection)

  map_names(kinome_data, species_selection, kinase_names, aliases) %>%
    name_map_df2rhot(aliases)
}

#' Download the results of the name map
#'
#' @description Helper function to handle downloads of the name map.
#'
#' @param mapped_kinases_rhot An rhot object containing the mapped kinases (created with `name_map_rhot()`)
#'
#' @noRd
name_map_dl_helper <- function(mapped_kinases_rhot) {
  if(!is.null(mapped_kinases_rhot)) {
    mapped_kinases_rhot %>%
      rhandsontable::hot_to_r() %>%
      dplyr::filter(.data$Include == TRUE) %>%
      dplyr::select(-"Include")
  } else {
    return(data.frame(
      Input = character(),
      Manning_Name = character(),
      Aliases = character()
    ))
  }
}
