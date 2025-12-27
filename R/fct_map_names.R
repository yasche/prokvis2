#' map_names
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
map_names <- function(kinome_data, species_selection, kinase_names) {
  aliases <- extract_aliases_df(kinome_data, species_selection)

  input_kinases <- kinase_names %>%
    str_split("\\n") %>%
    unlist()

  matched_kinases <- purrr::map(input_kinases, grep, aliases$Alias, ignore.case = T)

  names(matched_kinases) <- input_kinases

  purrr::map(matched_kinases, select_rows_int, aliases) %>%
    dplyr::bind_rows(.id = "Input") %>%
    dplyr::transmute(Input = Input,
                     Manning_Name = Gene,
                     Aliases = Alias)
}

#' Helper function to select rows in a data frame by index
#'
#' @description A fct function
#'
#' @return a subset of `df` which only contains the `indices`.
#'
#' @noRd
#'
select_rows_int <- function(indices, df) {
  df[indices, ]
}

name_map_df2rhot <- function(name_map_df, aliases) {
  rhandsontable::rhandsontable(name_map_df, height = 500) %>%
    rhandsontable::hot_table(stretchH = "all") %>%
    rhandsontable::hot_col(1, readOnly = T) %>%
    rhandsontable::hot_col(2, type = "dropdown", source = aliases$Manning_Name, strict = TRUE) %>%
    rhandsontable::hot_col(3, type = "checkbox") %>%
    rhandsontable::hot_rows(rowHeights = 25)
}

#' Helper function to extract the alias df from kinome_data list
#'
#' @description A fct function
#'
#' @return A data frame containing the aliases for a given species..
#'
#' @noRd
extract_aliases_df <- function(kinome_data, species_selection) {
  kinome_data[[species_selection]]$aliases
}
