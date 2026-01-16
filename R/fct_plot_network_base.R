#' Create the base plot for the network tab
#'
#' @description Create the network base plot without any decoration.
#'
#' @param kinome_df A kinome data frame, created with `extract_kinome_df()`.
#' @param set_seed `numeric` or `NULL` - A random seed to reproduce the graph layout.
#'
#' @return A `network` object to be used as the network base plot.
#'
#' @noRd
plot_network_base <- function(kinome_df, set_seed) {
  reduced_kinome <- kinome_df %>%
    dplyr::select("Manning_Name", "Kinase_Group", "Kinase_Family", "Kinase_Subfamily") %>%
    dplyr::distinct()


  # rlang::is_na can handle NULL
  seed <- ifelse(rlang::is_na(set_seed) | rlang::is_null(set_seed), 1, set_seed)

  network_base <- withr::with_seed(seed, plot_network_base_helper(reduced_kinome))

  dplyr::left_join(network_base, reduced_kinome, by = dplyr::join_by("vertex.names" == "Manning_Name"))
}


#' Helper function to create the network base plot
#'
#' @description
#' This helper function is necessary to preserve the current random seed (i.e., not to disturb the user's R 'landscape').
#'
#' @param reduced_kinome `tibble` - A reduced kinome tibble with columns `Manning_Name`, `Kinase_Group`, `Kinase_Family`, `Kinase_Subfamily`
#'
#' @returns A `network` object.
#'
#' @noRd
plot_network_base_helper <- function(reduced_kinome) {
  reduced_kinome %>%
    kinome_df_to_igraph() %>%
    intergraph::asNetwork() %>%
    ggnetwork::ggnetwork() %>%
    dplyr::mutate(vertex.names = stringr::str_remove_all(.data$vertex.names, "^x[0-9]\\.[0-9][0-9]_"))
}
