#' kinome_to_igraph
#'
#' @description Turns the selected kinome (via the `species_selection` argument) into an `igraph` object.
#'
#' @param kinome_data A `kinome_data` object
#' @param species_selection The two-letter code of the selected species.
#'
#' @return A network representation of the kinome as an `igraph` object.
#'
#' @noRd
kinome_to_igraph <- function(kinome_data, species_selection) {
  kinome_df <- extract_kinome_df(kinome_data, species_selection)

  kinome_df_to_igraph_helper(kinome_df)
}

#' kinome_df_to_igraph_helper
#'
#' @description helper function to be used with `kinome_to_igraph`
#'
#' @param kinome_df A kinome data frame of the selected species
#'
#' @return A network representation of the kinome as an `igraph` object.
#'
#' @noRd
kinome_df_to_igraph_helper <- function(kinome_df) {
  #'Group ', 'Family ', 'Subfamily 'are because sometimes kinase name and subfamily etc are the same. this avoids circular connections

  #Branch length is added as metadata to later be extracted:
  #Origin -> Group -> Family -> Subfamily -> Kinase
  #        .25    .25        .25          .25
  #Origin -> Group -> Family ->              Kinase
  #        .25    .25        .5


  #create origin -> group sub_df
  o2g <- kinome_df %>%
    dplyr::select("Kinase_Group") %>%
    dplyr::transmute(from = "Origin",
                     to = paste("x0.25_", "Group ", .data$Kinase_Group, sep = "")) %>%
    dplyr::distinct()

  #create group -> family sub_df
  g2f <- kinome_df %>%
    dplyr::select("Kinase_Group", "Kinase_Family") %>%
    dplyr::transmute(from = paste("x0.25_", "Group ", .data$Kinase_Group, sep = ""),
                     to = paste("x0.25_", "Family ", .data$Kinase_Family, sep = "")) %>%
    dplyr::distinct()

  #create family -> kinase sub_df (kinases that do not have a subfamily)
  f2k <- kinome_df %>%
    dplyr::filter(is.na(.data$Kinase_Subfamily)) %>%
    dplyr::select("Kinase_Family", "Manning_Name") %>%
    dplyr::transmute(from = paste("x0.25_", "Family ", .data$Kinase_Family, sep = ""),
                     to = paste("x0.50_", .data$Manning_Name, sep = "")) %>%
    dplyr::distinct()

  #create family -> subfamily sub_df
  f2s <- kinome_df %>%
    dplyr::filter(!is.na(.data$Kinase_Subfamily)) %>%
    dplyr::select("Kinase_Family", "Kinase_Subfamily") %>%
    dplyr::transmute(from = paste("x0.25_", "Family ", .data$Kinase_Family, sep = ""),
                     to = paste("x0.25_", "Subfamily ", .data$Kinase_Subfamily, sep = "")) %>%
    dplyr::distinct()

  #create subfamily -> kinase sub_df (kinases that do have a subfamily)
  s2k <- kinome_df %>%
    dplyr::filter(!is.na(.data$Kinase_Subfamily)) %>%
    dplyr::select("Kinase_Subfamily", "Manning_Name") %>%
    dplyr::transmute(from = paste("x0.25_", "Subfamily ", .data$Kinase_Subfamily, sep = ""),
                     to = paste("x0.25_", .data$Manning_Name, sep = "")) %>%
    dplyr::distinct()

  igdf <- rbind(o2g, g2f, f2s, f2k, s2k)

  igraph::graph_from_data_frame(igdf, directed = F)
}
