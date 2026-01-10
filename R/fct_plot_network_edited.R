#' plot_network_edited
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
plot_network_edited <- function(circular_base,
                                combined_nodes_and_edges,
                                selected_kinome) {
}

assign_branch_groups_network <- function(kinome_df, network_base) {
  kinome_df <- kinome_df %>%
    dplyr::select("Manning_Name", "Kinase_Group", "Kinase_Family", "Kinase_Subfamily") %>%
    dplyr::mutate(Kinase_Group = paste0("Group ", .data$Kinase_Group),
                  Kinase_Family = paste0("Family ", .data$Kinase_Family),
                  Kinase_Subfamily = paste0("Subfamily ", .data$Kinase_Subfamily)) %>%
    dplyr::mutate(Kinase_Subfamily = dplyr::case_when(.data$Kinase_Subfamily == "Subfamily NA" ~ NA,
                                                      .default = .data$Kinase_Subfamily))

  nwmp_kinases <- network_base %>%
    dplyr::filter(!is.na(.data$Kinase_Group))

  nwmp_groups <- network_base %>%
    dplyr::filter(grepl("Group ", .data$vertex.names)) %>%
    dplyr::mutate(Kinase_Group = stringr::str_remove(.data$vertex.names, "Group "))

  nwmp_families <- network_base %>%
    dplyr::filter(grepl("Family ", .data$vertex.names)) %>%
    dplyr::select(-"Kinase_Group") %>%
    dplyr::left_join(dplyr::distinct(dplyr::select(.env$kinome_df, -"Manning_Name", -"Kinase_Subfamily")), by = dplyr::join_by("vertex.names" == "Kinase_Family")) %>%
    dplyr::distinct()

  nwmp_subfamilies <- network_base %>%
    dplyr::filter(grepl("Subfamily ", .data$vertex.names)) %>%
    dplyr::select(-"Kinase_Group") %>%
    dplyr::left_join(dplyr::distinct(dplyr::select(.env$kinome_df, -"Manning_Name", -"Kinase_Family")), by = dplyr::join_by("vertex.names" == "Kinase_Subfamily")) %>%
    dplyr::distinct()

  nwmp_origin <- network_base %>%
    dplyr::filter(.data$vertex.names == "Origin") %>%
    #add connection origin -> group
    dplyr::select(-"Kinase_Group") %>%
    dplyr::left_join(dplyr::distinct(dplyr::select(.env$nwmp_groups, "x", "y", "Kinase_Group")), by = dplyr::join_by("xend" == "x", "yend" == "y"))


  rbind(nwmp_kinases,
        nwmp_groups,
        nwmp_families,
        nwmp_subfamilies,
        nwmp_origin) %>%
    dplyr::mutate(Kinase_Group = stringr::str_remove(.data$Kinase_Group, "Group "))
}
