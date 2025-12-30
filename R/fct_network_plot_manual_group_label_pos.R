#' manual_group_label_pos_input
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
manual_group_label_pos_input <- function(custom_xy, kinase_groups, ns, id) {
  #partly adapted from https://mastering-shiny.org/action-dynamic.html
  id_label <- paste0(id, "-custom_group_pos_", collapse = "")

  purrr::map(ns(custom_xy), ~ shiny::numericInput(
    .x,
    paste(kinase_groups[as.numeric(stringr::str_remove(.x, paste0(id_label, "x", "|", id_label, "y")))], stringr::str_remove_all(.x, paste0(id_label, "|[0-9]", collapse = ""))),
    value = 0,
    step = 0.05)
  )
}

kinase_groups_to_custom_xy <- function(kinase_groups) {
  seq_len_kinase_groups <- 1:length(kinase_groups)

  posx <- paste0("custom_group_pos_x", seq_len_kinase_groups)
  posy <- paste0("custom_group_pos_y", seq_len_kinase_groups)

  as.vector(rbind(posx, posy))
}
