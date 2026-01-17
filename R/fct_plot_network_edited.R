#' Create the final plot for the network tab
#'
#' @description Decorate the network base plot created with `plot_network_base()` with user-supplied values.
#'
#' @param network_base The base plot created with `plot_network_base()`
#' @param combined_nodes_and_edges A table containing information on node and edge appearance, created with `combine_nodes_and_edges()`
#' @param selected_kinome The kinome data frame
#' @param color_branches_groups `logical` - should branches be colored based on groups?
#' @param branch_thickness `numeric` - branch thickness
#' @param default_branch_color `character` - if `color_branches_groups = FALSE`, what color should be used for the branches?
#' @param color_kinase_edges_groups `logical` - should kinase edges be colored based on their groups? Overwrites the colors supplied with `combined_nodes_and_edges()` if set to `TRUE`
#' @param show_kinases_labels `character` - which kinase labels should be shown? One of `c("All", "None", "Annotated", "Manual selection")`
#' @param kinase_labels_manual_selection `character` - Name of the kinases whose labels should be shown if `show_kinases_labels = "Manual selection"`
#' @param kinase_edges_hot `data.frame` - containing kinase edges
#' @param nudge_kinase_label_x `numeric` - Manual nudge for kinase labels in the x direction
#' @param nudge_kinase_label_y `numeric` - Manual nudge for kinase labels in the y direction
#' @param show_which_kinase_labels `character` - the source of the kinase names. One of `c("Manning Name", "Uniprot gene name", "Uniprot entry", "Uniprot kinase name", "Uniprot accession", "Custom")`. If `"Custom"`, a Clabel for each kinase must be supplied in `combine_nodes_and_edges()`.
#' @param color_kinase_labels_groups `logical` - should kinase labels be colored based on groups?
#' @param label_size `numeric` - the kinase label size
#' @param default_label_color `character` - which color should be used for kinase labels? Only necessary if `color_kinase_labels_groups = FALSE`
#' @param highlight_groups `logical` - If `TRUE` the Kinase Groups are highlighted in a specific color.
#' @param group_highlighter_alpha `numeric` - Only necessary if `highlight_groups = TRUE`; the alpha used for the group highlighter.
#' @param color_palette `character` - The name of a color palette from the `scico` package, `"Custom"` or `"Default ggplot2"`
#' @param custom_color_pal A reactive created with function `custom_color_nums_to_pal()`
#' @param custom_xy_nudge `tibble` - contains the manual nudge for each Kinase Group label.
#' @param show_group_labels `logical` - Should group labels be displayed?
#' @param group_label_size `numeric` - The group label size.
#' @param legend_label_size `numeric` - The font size used in the legend labels.
#' @param legend_title_size `numeric` - The font size used in the legend title.
#' @param hide_legend `logical` - If `TRUE` the legend is hidden, if `FALSE` it is displayed.
#'
#' @return The final plot as a `ggplot` object.
#'
#' @noRd
plot_network_edited <- function(network_base,
                                combined_nodes_and_edges,
                                selected_kinome,
                                color_branches_groups,
                                branch_thickness,
                                default_branch_color,
                                color_kinase_edges_groups,
                                show_kinases_labels,
                                kinase_labels_manual_selection,
                                kinase_edges_hot,
                                nudge_kinase_label_x,
                                nudge_kinase_label_y,
                                show_which_kinase_labels,
                                color_kinase_labels_groups,
                                label_size,
                                default_label_color,
                                highlight_groups,
                                group_highlighter_alpha,
                                color_palette,
                                custom_color_pal,
                                custom_xy_nudge,
                                show_group_labels,
                                group_label_size,
                                legend_label_size,
                                legend_title_size,
                                hide_legend) {

  group_label_size <- group_label_size %||% 5

  combined_nodes_and_edges <- combined_nodes_and_edges %>%
    dplyr::mutate(Name = dplyr::case_when(.data$id == "Group" ~ stringr::str_replace_all(.data$Name, "^Group_", "Group "),
                                          .data$id == "Family" ~ stringr::str_replace_all(.data$Name, "^Family_", "Family "),
                                          .data$id == "Subfamily" ~ stringr::str_replace_all(.data$Name, "^Subfamily_", "Subfamily "),
                                          .default = .data$Name))

  network_base <- network_base %>%
    dplyr::left_join(combined_nodes_and_edges, by = dplyr::join_by("vertex.names" == "Name")) %>%
    dplyr::left_join(dplyr::select(selected_kinome, "Manning_Name", "Uniprot_Gene_Name", "Uniprot_Entry", "Kinase_Name", "Uniprot_Accession"), by = dplyr::join_by("vertex.names" == "Manning_Name"), multiple = "first")

  # check if branches should be colored based on groups
  if (color_branches_groups == TRUE) {

    network_base <- assign_branch_groups_network(kinome_df = selected_kinome,
                                                 network_base = network_base)

    p <- ggplot2::ggplot(network_base, ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend)) +
      ggnetwork::geom_edges(ggplot2::aes(color = .data$Kinase_Group), size = branch_thickness)

  } else {
    p <- ggplot2::ggplot(network_base, ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend)) +
      ggnetwork::geom_edges(color = default_branch_color, size = branch_thickness)

  }

  if(color_kinase_edges_groups == FALSE) {
    p <- p + ggplot2::geom_point(size = p$data$Size,
                                 fill = p$data$Color,
                                 pch = p$data$Shape,
                                 stroke = p$data$Stroke_Width,
                                 color = p$data$Stroke)
  } else {
    p <- p + ggplot2::geom_point(ggplot2::aes(fill = .data$Kinase_Group, color = .data$Kinase_Group), size = p$data$Size, pch = p$data$Shape, stroke = p$data$Stroke_Width)
  }
  # create a copy of label so that geom_tiplab and geom_point_interactive can have different values
  # (important for hiding kinases)
  p$data$vertex.names2 <- p$data$vertex.names

  # remove family and subfamily labels (for now)

  p$data <- p$data %>%
    dplyr::mutate(vertex.names2 = dplyr::case_when(.data$id == "Kinase" ~ .data$vertex.names2,
                                                   .default = NA))

  # check which kinase labels to show
  if (show_kinases_labels == "None") {
    p$data$vertex.names2 <- NA
  } else if(show_kinases_labels == "Manual selection") {
    p$data <- p$data %>%
      dplyr::mutate(vertex.names2 = dplyr::case_when(.data$vertex.names2 %in% .env$kinase_labels_manual_selection ~ .data$vertex.names2,
                                                     .default = NA))
  } else if(show_kinases_labels == "Annotated") {
    p$data <- p$data %>%
      dplyr::mutate(vertex.names2 = dplyr::case_when(.data$vertex.names2 %in% rhandsontable::hot_to_r(.env$kinase_edges_hot)$Name ~ .data$vertex.names2,
                                                     .default = NA))
  }

  # catch missing value for nudgeX and nudgeY
  if (rlang::is_na(nudge_kinase_label_x) | rlang::is_null(nudge_kinase_label_x)) {
    nudge_kinase_label_x <- 0
  }

  if (rlang::is_na(nudge_kinase_label_y) | rlang::is_null(nudge_kinase_label_y)) {
    nudge_kinase_label_y <- 0
  }

  # manipulate label here
  if (show_which_kinase_labels == "Uniprot gene name") {
    p$data <- p$data %>%
      dplyr::mutate(Uniprot_Gene_Name = dplyr::case_when(is.na(.data$vertex.names2) ~ NA,
                                                         .default = .data$Uniprot_Gene_Name)) %>%
      dplyr::mutate(chosen_label = .data$Uniprot_Gene_Name) %>%
      dplyr::mutate(chosen_label = dplyr::case_when(.data$id == "Group" | .data$id == "Family" | .data$id == "Subfamily" | .data$id == "Origin" ~ .data$label,
                                                    .default = .data$chosen_label)) %>%
      dplyr::mutate(vertex.names2 = .data$chosen_label) %>%
      dplyr::select(-"chosen_label")
  } else if (show_which_kinase_labels == "Uniprot entry") {
    p$data <- p$data %>%
      dplyr::mutate(Uniprot_Entry = dplyr::case_when(is.na(.data$vertex.names2) ~ NA,
                                                     .default = .data$Uniprot_Entry)) %>%
      dplyr::mutate(chosen_label = .data$Uniprot_Entry) %>%
      #preserve group, family, subfamily
      dplyr::mutate(chosen_label = dplyr::case_when(.data$id == "Group" | .data$id == "Family" | .data$id == "Subfamily" | .data$id == "Origin" ~ .data$label,
                                                    .default = .data$chosen_label)) %>%
      dplyr::mutate(vertex.names2 = .data$chosen_label) %>%
      dplyr::select(-"chosen_label")
  } else if (show_which_kinase_labels == "Uniprot kinase name") {
    p$data <- p$data %>%
      dplyr::mutate(Kinase_Name = dplyr::case_when(is.na(.data$vertex.names2) ~ NA,
                                                   .default = .data$Kinase_Name)) %>%
      dplyr::mutate(chosen_label = .data$Kinase_Name) %>%
      #preserve group, family, subfamily
      dplyr::mutate(chosen_label = dplyr::case_when(.data$id == "Group" | .data$id == "Family" | .data$id == "Subfamily" | .data$id == "Origin" ~ .data$label,
                                                    .default = .data$chosen_label)) %>%
      dplyr::mutate(vertex.names2 = .data$chosen_label) %>%
      dplyr::select(-"chosen_label")
  } else if (show_which_kinase_labels == "Uniprot accession") {
    p$data <- p$data %>%
      dplyr::mutate(Uniprot_Accession = dplyr::case_when(is.na(.data$vertex.names2) ~ NA,
                                                         .default = .data$Uniprot_Accession)) %>%
      dplyr::mutate(chosen_label = .data$Uniprot_Accession) %>%
      #preserve group, family, subfamily
      dplyr::mutate(chosen_label = dplyr::case_when(.data$id == "Group" | .data$id == "Family" | .data$id == "Subfamily" | .data$id == "Origin" ~ .data$label,
                                                    .default = .data$chosen_label)) %>%
      dplyr::mutate(vertex.names2 = .data$chosen_label) %>%
      dplyr::select(-"chosen_label")
  } else if (show_which_kinase_labels == "Custom") {
    p$data <- p$data %>%
      dplyr::mutate(Clabel = dplyr::case_when(is.na(.data$vertex.names2) ~ NA,
                                              .default = .data$Clabel)) %>%
      dplyr::mutate(chosen_label = .data$Clabel) %>%
      #preserve group, family, subfamily
      dplyr::mutate(chosen_label = dplyr::case_when(.data$id == "Group" | .data$id == "Family" | .data$id == "Subfamily" | .data$id == "Origin" ~ .data$label,
                                                    .default = .data$chosen_label)) %>%
      dplyr::mutate(vertex.names2 = .data$chosen_label) %>%
      dplyr::select(-"chosen_label")
  }


  # check if kinase labels should be colored based on group.
  if (color_kinase_labels_groups == TRUE) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = .data$vertex.names2, color = .data$Kinase_Group), size = label_size, nudge_x = nudge_kinase_label_x, nudge_y = nudge_kinase_label_y)#, family = input$chosenFont)
  } else {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = .data$vertex.names2), size = label_size, color = default_label_color, nudge_x = nudge_kinase_label_x, nudge_y = nudge_kinase_label_y)#, family = input$chosenFont)
  }

  # to color kinases and GFS individually, a new column is created.
  # it contains labels only for kinases and NA for GFS
  p$data <- p$data %>%
    dplyr::mutate(Kinase_Group2 = dplyr::case_when(.data$id == "Kinase" ~ .data$Kinase_Group,
                                                   .default = NA))

  # add interactivity if chosen
  # remove interactivity for now
  #if(input$staticInteractive == "interactive"){
  #  p <- p +
  #    theme_blank()+
  #    geom_point_interactive(aes(tooltip = vertex.names, data_id = vertex.names), alpha = 0)
  #} else {
  p <- p + ggplot2::theme(aspect.ratio = 1)
  #}

  if (!rlang::is_null(highlight_groups)) {
    if (highlight_groups == TRUE) {
      # to highlight groups:
      p <- p + ggplot2::stat_ellipse(ggplot2::aes(fill = .data$Kinase_Group), geom = "polygon", alpha = group_highlighter_alpha)
    }
  }

  # change color palette
  if (color_palette != "Default ggplot2") {
    if (color_palette == "Custom") {
      p <- p + ggplot2::scale_fill_manual(values = custom_color_pal, aesthetics = c("colour", "fill"), na.translate = F)
    } else {
      p <- p + scico::scale_color_scico_d(palette = color_palette, aesthetics = c("colour", "fill"), na.translate = F)
    }
  } else {
    p <- p + ggplot2::scale_color_discrete(aesthetics = c("colour", "fill"), na.translate = F)
  }

  # calculate position of group labels
  coord_origin <- p$data %>%
    dplyr::filter(.data$id == "Origin") %>%
    dplyr::select("x", "y", "vertex.names") %>%
    dplyr::distinct()

  group_labels <- p$data %>%
    dplyr::filter(.data$id == "Group") %>%
    dplyr::select("x", "y", "vertex.names") %>%
    dplyr::distinct() %>%
    dplyr::mutate(x_dist = .data$x - .env$coord_origin$x,
                  y_dist = .data$y - .env$coord_origin$y) %>%
    dplyr::mutate(x_pos = .env$coord_origin$x + .data$x_dist * 2,
                  y_pos = .env$coord_origin$y + .data$y_dist * 2) %>%
    dplyr::transmute(label = stringr::str_remove_all(.data$vertex.names, "^Group "),
                     x = .data$x_pos,
                     y = .data$y_pos)


  if (show_group_labels == TRUE) {
    # add individual nudge
    group_labels <- group_labels %>%
      dplyr::left_join(custom_xy_nudge, by = "label") %>%
      dplyr::mutate(x = .data$x + .data$x_nudge,
                    y = .data$y + .data$y_nudge)
    p <- p + ggplot2::geom_text(data = group_labels, ggplot2::aes(x = .data$x, y = .data$y, color = .data$label, label = .data$label),  size = group_label_size, inherit.aes = FALSE)#, family = input$chosenFont)
  }



  # lines are necessary to have only 1 legend
  p <- p + ggplot2::labs(colour = "Kinase Group")

  if (!is.null(highlight_groups)) {
    if (highlight_groups == TRUE) {

      # prevent message
      # Ignoring unknown labels:
      # fill : "Kinase Group"
      p <- p + ggplot2::labs(fill = "Kinase Group")
    }
  }

  p <- p + ggplot2::labs(dictionary = c(Kinase_Group = "Kinase Group"))

  #interactive mode can't handle other fonts in legend for some reason...
  #if (input$staticInteractive == "static") {
  p <- p + ggplot2::theme(legend.text = ggplot2::element_text(size = legend_label_size),# family = input$chosenFont),
                          legend.title = ggplot2::element_text(size = legend_title_size))#, family = input$chosenFont))
  #}

  if(hide_legend == TRUE) {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  p + ggplot2::theme(panel.background = ggplot2::element_blank(),
                     axis.title = ggplot2::element_blank(),
                     axis.text = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank())
}

#' Assign branch group for network plot
#'
#' @description
#' Add branch groups for kinases, mainly used to set the color for plotting.
#'
#' @param kinome_df A kinome df, created with `extract_kinome_df()`
#' @param network_base The base plot created with `plot_network_base()`
#'
#' @returns A tibble were each kinase is assigned their group.
#'
#' @noRd
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
    dplyr::left_join(dplyr::distinct(dplyr::select(kinome_df, -"Manning_Name", -"Kinase_Subfamily")), by = dplyr::join_by("vertex.names" == "Kinase_Family")) %>%
    dplyr::distinct()

  nwmp_subfamilies <- network_base %>%
    dplyr::filter(grepl("Subfamily ", .data$vertex.names)) %>%
    dplyr::select(-"Kinase_Group") %>%
    dplyr::left_join(dplyr::distinct(dplyr::select(kinome_df, -"Manning_Name", -"Kinase_Family")), by = dplyr::join_by("vertex.names" == "Kinase_Subfamily")) %>%
    dplyr::distinct()

  nwmp_origin <- network_base %>%
    dplyr::filter(.data$vertex.names == "Origin") %>%
    # add connection origin -> group
    dplyr::select(-"Kinase_Group") %>%
    dplyr::left_join(dplyr::distinct(dplyr::select(nwmp_groups, "x", "y", "Kinase_Group")), by = dplyr::join_by("xend" == "x", "yend" == "y"))


  rbind(nwmp_kinases,
        nwmp_groups,
        nwmp_families,
        nwmp_subfamilies,
        nwmp_origin) %>%
    dplyr::mutate(Kinase_Group = stringr::str_remove(.data$Kinase_Group, "Group "))
}
