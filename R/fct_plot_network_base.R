#' plot_network_base
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
plot_network_base <- function(kinome_df, set_seed) {
  reduced_kinome <- kinome_df %>%
    dplyr::select("Manning_Name", "Kinase_Group", "Kinase_Family", "Kinase_Subfamily") %>%
    dplyr::distinct()


  seed <- ifelse(is.na(set_seed) | rlang::is_null(set_seed), 1, set_seed)

  network_base <- withr::with_seed(seed, plot_network_base_helper(reduced_kinome))

  dplyr::left_join(network_base, reduced_kinome, by = dplyr::join_by("vertex.names" == "Manning_Name"))
}


plot_network_base_helper <- function(reduced_kinome) {
  reduced_kinome %>%
    kinome_df_to_igraph() %>%
    intergraph::asNetwork() %>%
    ggnetwork::ggnetwork() %>%
    dplyr::mutate(vertex.names = stringr::str_remove_all(.data$vertex.names, "^x[0-9]\\.[0-9][0-9]_"))
}
