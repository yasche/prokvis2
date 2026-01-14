#' Create the input for custom group colors
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
    # partly adapted from https://mastering-shiny.org/action-dynamic.html
    "Choose a custom color for each group",
    shiny::HTML("<br><br>"),
    # paste0(id, "-custom_group_col", collapse = "") hack to account for ns()
    purrr::map(ns(custom_color_nums), ~ colourpicker::colourInput(.x, kinase_groups[as.numeric(stringr::str_remove(.x, paste0(id, "-custom_group_col", collapse = "")))], value = "#BEBEBE"))
  )
}

#' Extract kinase groups from a given species from the `kinome_data` object
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

#' Encode kinase groups with prefix, followed by a unique number
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


#' Extract custom colors for each kinase group from the `input`
#'
#' @description Extract the custom colors created with `kinase_groups_to_custom_color_numbers()` for each kinase group from `input` and return them as a character vector
#'
#' @param custom_color_nums The unique color numbers for each kinase group, created with `kinase_groups_to_custom_color_numbers()`
#'
#' @description Convert custom color numbers to a custom palette
#'
#' @return A character vector containing the custom group colors
#'
#' @noRd
custom_color_nums_to_pal <- function(custom_color_nums, input) {
  cols <- purrr::map_chr(custom_color_nums, ~ input[[.x]] %||% "")
  # convert empty inputs to transparent
  cols[cols == ""] <- NA

  cols
}
