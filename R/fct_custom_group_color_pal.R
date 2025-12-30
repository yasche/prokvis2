#' custom_group_color_input
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
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

extract_kinase_groups <- function(kinome_df) {
  kinome_df %>%
    pull("Kinase_Group") %>%
    unique()
}

kinase_groups_to_custom_color_numbers <- function(kinase_groups) {
  paste0("custom_group_col", seq_len(length(kinase_groups)))
}

