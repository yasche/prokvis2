#' network_plot_manual_group_label_pos
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
network_plot_manual_group_label_pos <- function() {
}

kinase_groups_to_custom_xy <- function(kinase_groups) {
  seq_len_kinase_groups <- 1:length(kinase_groups)

  posx <- paste0("custom_group_pos_x", seq_len_kinase_groups)
  posy <- paste0("custom_group_pos_y", seq_len_kinase_groups)

  #c(paste0("posx", seq_len_kinase_groups), paste0("posy", seq_len_kinase_groups)) #%>%
    #matrix(ncol = 2, byrow = F) %>%
    #t() %>%
    #c()
  as.vector(rbind(posx, posy))
}
