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
