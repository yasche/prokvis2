#' nodes_and_edges
#'
#' @description Create a rhot to manually change node and edge behavior.
#'
#' @param kinome_df A kinome data frame created with `extract_kinome_df()`
#' @param which_ne A character defining which node or edge to create a rhot for: c("Manning_Name", "Kinase_Subfamily", "Kinase_Family", "Kinase_Group").
#'
#' @return An rhot to manually edit node and edge behavior.
#'
#' @noRd
nodes_and_edges <- function(kinome_df, which_ne) {
  ne_names <- kinome_df %>%
    dplyr::select({{ which_ne }}) %>%
    dplyr::pull() %>%
    unique()

  ne_df_helper(ne_names) %>%
    ne_df_to_rhot_helper(ne_names)
}

#' ne_df_helper
#'
#' @description Create an empty data frame with `length(ne_names)` rows.
#'
#' @param ne_names A character vector containing the node & edge names
#'
#' @return An empty data frame with `length(ne_names)` rows.
#'
#' @noRd
ne_df_helper <- function(ne_names) {
  ne_names_length <- length(ne_names)

  data.frame(Name = character(length = ne_names_length),
             Size = numeric(length = ne_names_length),
             Color = character(length = ne_names_length),
             Shape = character(length = ne_names_length),
             Stroke = character(length = ne_names_length),
             Stroke_Width = numeric(length = ne_names_length),
             Clabel =  character(length = ne_names_length))
}

#' ne_df_helper
#'
#' @description Convert the data frame created with `ne_df_helper()` to an rhot.
#'
#' @param ne_df An empty data frame created with `ne_df_helper()`
#' @param ne_names A character vector containing the node & edge names
#'
#' @return An empty rhot.
#'
#' @noRd
ne_df_to_rhot_helper <- function(ne_df, ne_names) {
  ne_df %>%
    rhandsontable::rhandsontable(width = "100%") %>%
    rhandsontable::hot_table(stretchH = "all") %>%
    rhandsontable::hot_col(1, type = "dropdown", source = c("", ne_names)) %>%
    rhandsontable::hot_col(4, type = "dropdown", source = c("circle",
                                                            "square",
                                                            "diamond",
                                                            "triangle",
                                                            "inverted triangle"), strict = TRUE)
}


combine_nodes_and_edges <- function(kinase_edges_hot, group_nodes_hot, family_nodes_hot, subfamily_nodes_hot, kinome_df) {
  kinases <- ne_rhot_to_df_helper(kinase_edges_hot, "Manning_Name", kinome_df, NULL)
  kgroups <- ne_rhot_to_df_helper(kinase_edges_hot, "Kinase_Group", kinome_df, "Group")
  kfams <- ne_rhot_to_df_helper(kinase_edges_hot, "Kinase_Family", kinome_df, "Family")
  ksubfams <- ne_rhot_to_df_helper(kinase_edges_hot, "Kinase_Subfamily", kinome_df, "Subfamily")

  rbind(kinases, kgroups, kfams, ksubfams)
}

ne_rhot_to_df_helper <- function(ne_rhot, which_ne, kinome_df, prefix) {
  ne_df <- rhandsontable::hot_to_r(ne_rhot) %||% data.frame(Name = character(),
                                                            Size = numeric(),
                                                            Color = character(),
                                                            Shape = character(),
                                                            Stroke = character(),
                                                            Stroke_Width = numeric(),
                                                            Clabel =  character())

  ne_names <- kinome_df %>%
    dplyr::select({{ which_ne }}) %>%
    dplyr::distinct()

  ne_df <- dplyr::left_join(ne_names, ne_df, by = dplyr::join_by({{ which_ne }} == "Name"), multiple = "first")

  if (is.null(prefix)) {
    ne_df_names <- ne_df[[which_ne]]
  } else {
    ne_df_names <- paste(prefix, ne_df[[which_ne]], sep = "_")
  }

  ne_df %>%
    dplyr::transmute(Name = .env$ne_df_names,
                     Size = .data$Size,
                     Shape = .data$Shape,
                     Color = .data$Color,
                     Stroke = .data$Stroke,
                     Stroke_Width = .data$Stroke_Width,
                     Clabel = .data$Clabel) %>%
    dplyr::mutate(Size = dplyr::case_when(Size == 0 ~ NA, .default = .data$Size)) %>%
    dplyr::mutate(Color = dplyr::case_when(Color == "" ~ "#000000", .default = .data$Color)) %>%
    dplyr::mutate(Shape = dplyr::case_when(Shape == "" ~ "circle", .default = .data$Shape)) %>%
    dplyr::mutate(Shape = pointshape(.data$Shape)) %>%
    dplyr::mutate(Stroke = dplyr::case_when(Stroke == "" ~ Color, .default = .data$Stroke))
}


pointshape <- function(shape) {
  stringr::str_replace_all(shape, c("circle" = "21",
                                    "square" = "22",
                                    "diamond" = "23",
                                    "inverted triangle" = "25",
                                    "triangle" = "24")) %>%
    as.numeric()
}
