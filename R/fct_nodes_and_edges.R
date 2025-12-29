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


combine_nodes_and_edges <- function(kinase_edges_hot, group_nodes_hot, family_nodes_hot, subfamily_nodes_hot) {

  kinases <- hot_to_r(input$kinase_edges_hot) %||% data.frame(Name = character(),
                                                              Size = numeric(),
                                                              Color = character(),
                                                              Shape = character(),
                                                              Stroke = character(),
                                                              Stroke_Width = numeric(),
                                                              Clabel =  character())
  kin_names <- selected_kinome() %>%
    select(Manning_Name) %>%
    distinct()

  kinases <- left_join(kin_names, kinases, by = join_by(Manning_Name == Name)) %>%
    transmute(Name = Manning_Name,
              Size = Size,
              Shape = Shape,
              Color = Color,
              Stroke = Stroke,
              Stroke_Width = Stroke_Width,
              Clabel = Clabel) %>%
    mutate(Size = case_when(Size == 0 ~ NA,
                            .default = Size)) %>%
    mutate(Color = case_when(Color == "" ~ "#000000",
                             .default = Color)) %>%
    mutate(Shape = case_when(Shape == "" ~ "circle",
                             .default = Shape)) %>%
    mutate(Shape = pointshape(Shape)) %>%
    mutate(Stroke = case_when(Stroke == "" ~ Color,
                              .default = Stroke))

  #groups
  kgroups <- hot_to_r(input$group_nodes_hot) %||% data.frame(Name = character(),
                                                             Size = numeric(),
                                                             Color = character(),
                                                             Shape = character(),
                                                             Stroke = character(),
                                                             Stroke_Width = numeric(),
                                                             Clabel =  character())
  group_names <- selected_kinome() %>%
    select(Kinase_Group) %>%
    distinct()


  kgroups <- left_join(group_names, kgroups, by = join_by(Kinase_Group == Name)) %>%
    transmute(Name = paste("Group", Kinase_Group),
              Size = Size,
              Shape = Shape,
              Color = Color,
              Stroke = Stroke,
              Stroke_Width = Stroke_Width,
              Clabel = Clabel) %>%
    mutate(Size = case_when(Size == 0 ~ NA,
                            .default = Size)) %>%
    mutate(Color = case_when(Color == "" ~ "#000000",
                             .default = Color)) %>%
    mutate(Shape = case_when(Shape == "" ~ "circle",
                             .default = Shape)) %>%
    mutate(Shape = pointshape(Shape)) %>%
    mutate(Stroke = case_when(Stroke == "" ~ Color,
                              .default = Stroke))

  #families
  kfams <- hot_to_r(input$family_nodes_hot) %||% data.frame(Name = character(),
                                                            Size = numeric(),
                                                            Color = character(),
                                                            Shape = character(),
                                                            Stroke = character(),
                                                            Stroke_Width = numeric(),
                                                            Clabel =  character())
  family_names <- selected_kinome() %>%
    select(Kinase_Family) %>%
    distinct()


  kfams <- left_join(family_names, kfams, by = join_by(Kinase_Family == Name)) %>%
    transmute(Name = paste("Family", Kinase_Family),
              Size = Size,
              Shape = Shape,
              Color = Color,
              Stroke = Stroke,
              Stroke_Width = Stroke_Width,
              Clabel = Clabel)  %>%
    mutate(Size = case_when(Size == 0 ~ NA,
                            .default = Size)) %>%
    mutate(Color = case_when(Color == "" ~ "#000000",
                             .default = Color)) %>%
    mutate(Shape = case_when(Shape == "" ~ "circle",
                             .default = Shape)) %>%
    mutate(Shape = pointshape(Shape)) %>%
    mutate(Stroke = case_when(Stroke == "" ~ Color,
                              .default = Stroke))


  #subfamilies
  ksubfams <- hot_to_r(input$subfamily_nodes_hot) %||% data.frame(Name = character(),
                                                                  Size = numeric(),
                                                                  Color = character(),
                                                                  Shape = character(),
                                                                  Stroke = character(),
                                                                  Stroke_Width = numeric(),
                                                                  Clabel =  character())
  subfamily_names <- selected_kinome() %>%
    select(Kinase_Subfamily) %>%
    distinct()


  ksubfams <- left_join(subfamily_names, ksubfams, by = join_by(Kinase_Subfamily == Name)) %>%
    transmute(Name = paste("Subfamily", Kinase_Subfamily),
              Size = Size,
              Shape = Shape,
              Color = Color,
              Stroke = Stroke,
              Stroke_Width = Stroke_Width,
              Clabel = Clabel) %>%
    mutate(Size = case_when(Size == 0 ~ NA,
                            .default = Size)) %>%
    mutate(Color = case_when(Color == "" ~ "#000000",
                             .default = Color)) %>%
    mutate(Shape = case_when(Shape == "" ~ "circle",
                             .default = Shape)) %>%
    mutate(Shape = pointshape(Shape)) %>%
    mutate(Stroke = case_when(Stroke == "" ~ Color,
                              .default = Stroke))


  combined_df <- rbind(kinases, kgroups, kfams, ksubfams)
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

  ne_df <- dplyr::left_join(ne_names, ne_df, by = join_by({{ which_ne }} == "Name"), multiple = "first")

  if (is.null(prefix)) {
    ne_df_names <- ne_df[[which_ne]]
  } else {
    ne_df_names <- paste(prefix, ne_df[[which_ne]], sep = "_")
  }

  ne_df %>%
    dplyr::transmute(Name = .env$ne_df_names,
                     Size = Size,
                     Shape = Shape,
                     Color = Color,
                     Stroke = Stroke,
                     Stroke_Width = Stroke_Width,
                     Clabel = Clabel) %>%
    dplyr::mutate(Size = case_when(Size == 0 ~ NA, .default = Size)) %>%
    dplyr::mutate(Color = case_when(Color == "" ~ "#000000", .default = Color)) %>%
    dplyr::mutate(Shape = case_when(Shape == "" ~ "circle", .default = Shape)) %>%
    dplyr::mutate(Shape = pointshape(Shape)) %>%
    dplyr::mutate(Stroke = case_when(Stroke == "" ~ Color, .default = Stroke))
}


pointshape <- function(shape) {
  stringr::str_replace_all(shape, c("circle" = "21",
                                    "square" = "22",
                                    "diamond" = "23",
                                    "inverted triangle" = "25",
                                    "triangle" = "24")) %>%
    as.numeric()
}
