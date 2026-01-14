#' Create the input for custom group label position
#'
#' @description Wrapper function to create inputs in case a custom nudge for group labels is enabled in network plot.
#'
#' @param custom_xy  A character vector containing the groups encoded by a unique number and the prefix "custom_group_pos_x" or "custom_group_pos_y". Created with `kinase_groups_to_custom_xy()`.
#' @param kinase_groups A character vector containing the kinase group names. Created with `extract_kinase_groups()`.
#' @param ns The namespace `ns`.
#' @param id The module id.
#'
#' @return An input to be used in a UI.
#'
#' @noRd
manual_group_label_pos_input <- function(custom_xy, kinase_groups, ns, id) {
  # partly adapted from https://mastering-shiny.org/action-dynamic.html
  id_label <- paste0(id, "-custom_group_pos_", collapse = "")

  purrr::map(ns(custom_xy), ~ shiny::numericInput(
    .x,
    paste(kinase_groups[as.numeric(stringr::str_remove(.x, paste0(id_label, "x", "|", id_label, "y")))], stringr::str_remove_all(.x, paste0(id_label, "|[0-9]", collapse = ""))),
    value = 0,
    step = 0.05)
  )
}

#' Encode kinase groups with a prefix for x and y position, followed by a unique number
#'
#' @description Helper function to turn kinase groups into unique numbers with prefix "custom_group_pos_x" and "custom_group_pos_y".
#'
#' @param kinase_groups A character vector containing the kinase groups, created with `extract_kinase_groups()`
#'
#' @return A character vector containing the kinase groups encoded by a unique number and prefix "custom_group_pos_x" and "custom_group_pos_y".
#'
#' @noRd
kinase_groups_to_custom_xy <- function(kinase_groups) {
  seq_len_kinase_groups <- 1:length(kinase_groups)

  posx <- paste0("custom_group_pos_x", seq_len_kinase_groups)
  posy <- paste0("custom_group_pos_y", seq_len_kinase_groups)

  as.vector(rbind(posx, posy))
}

#' Extract custom x and y nudges for each kinase group from the `input`
#'
#' @description Extract the custom nudges created with `kinase_groups_to_custom_xy()` for each kinase group from `input` and return them as a tibble.
#'
#' @param custom_xy_nums The unique x and y numbers for each kinase group, created with `kinase_groups_to_custom_xy()`
#' @param input The `input` from the shiny server
#' @param kinase_groups A vector containing the kinase groups.
#'
#' @return A matrix containing the x and y nudge for each kinase group.
#'
#' @noRd
custom_xy_nums_to_nudge <- function(custom_xy_nums, input, kinase_groups) {
  xy <- purrr::map_dbl(custom_xy_nums, ~ input[[.x]] %||% 0)
  length_xy <- length(xy)

  xy <- matrix(xy, nrow = length_xy / 2, ncol = 2, byrow = TRUE)

  colnames(xy) <- c("x_nudge", "y_nudge")

  xy %>%
    tibble::as_tibble() %>%
    dplyr::mutate(label = .env$kinase_groups)
}
