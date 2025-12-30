#' custom_group_color_input
#'
#' @description Wrapper function to create the colourInputs for the individual groups if the palette is set to "Custom".
#'
#' @param custom_color_nums A character vector containing the groups encoded by a unique number and the prefix "custom_group_col". Created with `kinase_groups_to_custom_color_numbers()`.
#' @param kinase_groups A character vector containing the kinase group names. Created with `extract_kinase_groups()`.
#' @param ns The namespace `ns`.
#' @param id The module id.
#'
#' @return An input to be used in a UI.
#'
#' @noRd
custom_group_color_input <- function(custom_color_nums, kinase_groups, ns, id) {
  list(
    #partly adapted from https://mastering-shiny.org/action-dynamic.html
    "Choose a custom color for each group",
    shiny::HTML("<br><br>"),
    # paste0(id, "-custom_group_col", collapse = "") hack to account for ns()
    purrr::map(ns(custom_color_nums), ~ colourpicker::colourInput(.x, kinase_groups[as.numeric(stringr::str_remove(.x, paste0(id, "-custom_group_col", collapse = "")))], value = "#BEBEBE"))
  )
}

#' extract_kinase_groups
#'
#' @description Helper function to extract the kinase groups
#'
#' @param kinome_df A `kinome_df` created with `extract_kinome_df()`
#'
#' @return A character vector containing the kinase groups.
#'
#' @noRd
extract_kinase_groups <- function(kinome_df) {
  kinome_df %>%
    dplyr::pull("Kinase_Group") %>%
    unique()
}

#' kinase_groups_to_custom_color_numbers
#'
#' @description Helper function to turn kinase groups into unique numbers with prefix "custom_group_col".
#'
#' @param kinase_groups A character vector containing the kinase groups, created with `extract_kinase_groups()`
#'
#' @return A character vector containing the kinase groups encoded by a unique number and prefix "custom_group_col".
#'
#' @noRd
kinase_groups_to_custom_color_numbers <- function(kinase_groups) {
  paste0("custom_group_col", seq_len(length(kinase_groups)))
}

