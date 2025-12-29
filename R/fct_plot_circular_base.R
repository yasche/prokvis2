#' plot_circular_base
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
plot_circular_base <- function(kinome_df) {
  reduced_kinome <- kinome_df %>%
    dplyr::select("Manning_Name", "Kinase_Group", "Kinase_Family", "Kinase_Subfamily") %>%
    dplyr::distinct()

    circular_base <- reduced_kinome %>%
      kinome_df_to_igraph() %>%
      treeio::as.phylo() %>%
      tibble::as_tibble() %>%
      dplyr::mutate(branch.length = stringr::str_split_i(.data$label, "_", 1),
                    branch.length = stringr::str_remove(.data$branch.length, "x"),
                    branch.length = as.numeric(.data$branch.length)) %>%
      dplyr::mutate(label = stringr::str_remove(.data$label, "x[0-9].[0-9][0-9]_")) %>%
      treeio::as.treedata()

    treeio::full_join(circular_base, reduced_kinome, by = dplyr::join_by("label" == "Manning_Name"))
}
