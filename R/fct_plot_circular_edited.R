#' plot_circular_edited
#'
#' @param circular_base The base plot created with `plot_circular_base`
#' @param combined_nodes_and_edges A table containing information on node and edge appearance, created with `combine_nodes_and_edges`
#' @param selected_kinome The kinome data frame
#' @param color_branches_groups `logical` should branches be colored based on groups?
#' @param branch_thickness `numeric` branch thickness
#' @param default_branch_color `character` if `color_branches_groups = FALSE`, what color should be used for the branches?
#' @param color_kinase_edges_groups `logical` should kinase edges be colored based on their groups? Overwrites the colors supplied with `combined_nodes_and_edges` if set to `TRUE`
#' @param show_kinases_labels `character` which kinase labels should be shown? One of `c("All", "None", "Annotated", "Manual selection")`
#' @param kinase_labels_manual_selection `character` Name of the kinases whose labels should be shown if `show_kinases_labels = "Manual selection"`
#' @param kinase_edges_hot `data.frame` containing kinase edges
#' @param show_which_kinase_labels `character` the source of the kinase names. One of `c("Manning Name", "Uniprot gene name", "Uniprot entry", "Uniprot kinase name", "Uniprot accession", "Custom")`. If `"Custom"`, a Clabel for each kinase must be supplied in `combine_nodes_and_edges`.
#' @param color_kinase_labels_groups `logical` should kinase labels be colored based on groups?
#' @param label_size `numeric` the kinase label size
#' @param default_label_color `character` which color should be used for kinase labels? Only necessary if `color_kinase_labels_groups = FALSE`
#' @param mrcas A rective created with function `get_mrcas()`
#' @param color_palette `character` The name of a color palette from the `scico` package, `"Custom"` or `"Default ggplot2"`
#' @param custom_color_pal A reactive created with function `custom_color_nums_to_pal()`
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
plot_circular_edited <- function(circular_base,
                                 combined_nodes_and_edges,
                                 selected_kinome,
                                 color_branches_groups,
                                 branch_thickness,
                                 default_branch_color,
                                 color_kinase_edges_groups,
                                 show_kinases_labels,
                                 kinase_labels_manual_selection,
                                 kinase_edges_hot,
                                 show_which_kinase_labels,
                                 color_kinase_labels_groups,
                                 label_size,
                                 default_label_color,
                                 mrcas,
                                 color_palette,
                                 custom_color_pal,
                                 group_label_radius,
                                 show_group_labels,
                                 group_label_size,
                                 legend_label_size,
                                 legend_title_size,
                                 hide_legend,
                                 highlight_groups,
                                 group_highlighter_alpha
                                 ) {


  #print(group_nodes_hot, n = 10000)

  # put this in a separate function/reactive to speed up plotting?

  group_label_size <- group_label_size %||% 5
  group_label_radius <- group_label_radius %||% 1

  combined_nodes_and_edges <- combined_nodes_and_edges %>%
    dplyr::mutate(Name = dplyr::case_when(.data$id == "Group" ~ stringr::str_replace_all(.data$Name, "^Group_", "Group "),
                                          .data$id == "Family" ~ stringr::str_replace_all(.data$Name, "^Family_", "Family "),
                                          .data$id == "Subfamily" ~ stringr::str_replace_all(.data$Name, "^Subfamily_", "Subfamily "),
                                          .default = .data$Name))

  circular_base <- circular_base %>%
    dplyr::full_join(combined_nodes_and_edges, by = dplyr::join_by("label" == "Name")) %>%
    dplyr::left_join(dplyr::select(selected_kinome, "Manning_Name", "Uniprot_Gene_Name", "Uniprot_Entry", "Kinase_Name", "Uniprot_Accession"), by = dplyr::join_by("label" == "Manning_Name"), multiple = "first")

  if (color_branches_groups == TRUE) {
    p <- ggtree::ggtree(
      circular_base,
      layout = "fan",
      size = branch_thickness
    )
  } else {
    p <- ggtree::ggtree(
      circular_base,
      layout = "fan",
      size = branch_thickness,
      color = default_branch_color
    )
  }

  branch_groups <- assign_branch_groups_circular(kinome_df = selected_kinome,
                                                 circular_base = circular_base)

  p <- p %<+% branch_groups

  if(color_kinase_edges_groups == FALSE) {
    sizes <- c(combined_nodes_and_edges$Size)
    names(sizes) <- c(combined_nodes_and_edges$Name)

    colors <- combined_nodes_and_edges$Color
    names(colors) <- combined_nodes_and_edges$Name

    p <- p + ggplot2::geom_point(
      size = p$data$Size,
      fill = p$data$Color,
      pch = p$data$Shape,
      stroke = p$data$Stroke_Width,
      color = p$data$Stroke
    )

  } else {
    p <- p + ggplot2::geom_point(ggplot2::aes(fill = .data$glabel, color = .data$glabel), size = p$data$Size, pch = p$data$Shape, stroke = p$data$Stroke_Width)
  }
  # create a copy of label so that geom_tiplab and geom_point_interactive can have different values
  # (important for hiding kinases)
  p$data$label2 <- p$data$label

  #check which kinase labels to show
  if (show_kinases_labels == "None") {
    p$data$label <- NA
  } else if(show_kinases_labels == "Manual selection") {
    p$data <- p$data %>%
      dplyr::mutate(label = dplyr::case_when(.data$label %in% .env$kinase_labels_manual_selection ~ .data$label,
                                             .default = NA))
  } else if(show_kinases_labels == "Annotated") {
    p$data <- p$data %>%
      dplyr::mutate(label = dplyr::case_when(.data$label %in% rhandsontable::hot_to_r(.env$kinase_edges_hot)$Name ~ .data$label,
                                             .default = NA))
  }


  #manipulate label here
  if (show_which_kinase_labels == "Uniprot gene name") {

    p$data <- p$data %>%
      dplyr::mutate(Uniprot_Gene_Name = dplyr::case_when(is.na(.data$label) ~ NA,
                                                         .default = .data$Uniprot_Gene_Name)) %>%
      dplyr::mutate(chosen_label = .data$Uniprot_Gene_Name) %>%
      # safer way to mutate via id column
      #dplyr::mutate(chosen_label = dplyr::case_when(grepl("Origin|Group|Family|Subfamily", .data$label) ~ .data$label,
      #                                             .default = .data$chosen_label)) %>%
      dplyr::mutate(chosen_label = dplyr::case_when(.data$id == "Group" | .data$id == "Family" | .data$id == "Subfamily" | .data$id == "Origin" ~ .data$label,
                                                    .default = .data$chosen_label)) %>%
      dplyr::mutate(label = .data$chosen_label) %>%
      dplyr::select(-"chosen_label")

  } else if (show_which_kinase_labels == "Uniprot entry") {

    p$data <- p$data %>%
      dplyr::mutate(Uniprot_Entry = dplyr::case_when(is.na(.data$label) ~ NA,
                                                     .default = .data$Uniprot_Entry)) %>%
      dplyr::mutate(chosen_label = .data$Uniprot_Entry) %>%
      #preserve group, family, subfamily
      #dplyr::mutate(chosen_label = dplyr::case_when(grepl("Origin|Group|Family|Subfamily", .data$label) ~ .data$label,
      #                                              .default = .data$chosen_label)) %>%
      dplyr::mutate(chosen_label = dplyr::case_when(.data$id == "Group" | .data$id == "Family" | .data$id == "Subfamily" | .data$id == "Origin" ~ .data$label,
                                                    .default = .data$chosen_label)) %>%
      dplyr::mutate(label = .data$chosen_label) %>%
      dplyr::select(-"chosen_label")

  } else if (show_which_kinase_labels == "Uniprot kinase name") {

    p$data <- p$data %>%
      dplyr::mutate(Kinase_Name = dplyr::case_when(is.na(.data$label) ~ NA,
                                                   .default = .data$Kinase_Name)) %>%
      dplyr::mutate(chosen_label = .data$Kinase_Name) %>%
      #preserve group, family, subfamily
      #dplyr::mutate(chosen_label = dplyr::case_when(grepl("Origin|Group|Family|Subfamily", .data$label) ~ .data$label,
      #                                              .default = .data$chosen_label)) %>%
      dplyr::mutate(chosen_label = dplyr::case_when(.data$id == "Group" | .data$id == "Family" | .data$id == "Subfamily" | .data$id == "Origin" ~ .data$label,
                                                    .default = .data$chosen_label)) %>%
      dplyr::mutate(label = .data$chosen_label) %>%
      dplyr::select(-"chosen_label")

  } else if (show_which_kinase_labels == "Uniprot accession") {

    p$data <- p$data %>%
      dplyr::mutate(Uniprot_Accession = dplyr::case_when(is.na(.data$label) ~ NA,
                                                         .default = .data$Uniprot_Accession)) %>%
      dplyr::mutate(chosen_label = .data$Uniprot_Accession) %>%
      #preserve group, family, subfamily
      #dplyr::mutate(chosen_label = dplyr::case_when(grepl("Origin|Group|Family|Subfamily", .data$label) ~ .data$label,
      #                                              .default = .data$chosen_label)) %>%
      dplyr::mutate(chosen_label = dplyr::case_when(.data$id == "Group" | .data$id == "Family" | .data$id == "Subfamily" | .data$id == "Origin" ~ .data$label,
                                                    .default = .data$chosen_label)) %>%
      dplyr::mutate(label = .data$chosen_label) %>%
      dplyr::select(-"chosen_label")

  } else if (show_which_kinase_labels == "Custom") {

    p$data <- p$data %>%
      dplyr::mutate(Clabel = dplyr::case_when(is.na(.data$label) ~ NA,
                                              .default = .data$Clabel)) %>%
      dplyr::mutate(chosen_label = .data$Clabel) %>%
      #preserve group, family, subfamily
      #dplyr::mutate(chosen_label = dplyr::case_when(grepl("Origin|Group|Family|Subfamily", .data$label) ~ .data$label,
      #                                              .default = .data$chosen_label)) %>%
      dplyr::mutate(chosen_label = dplyr::case_when(.data$id == "Group" | .data$id == "Family" | .data$id == "Subfamily" | .data$id == "Origin" ~ .data$label,
                                                    .default = .data$chosen_label)) %>%
      dplyr::mutate(label = .data$chosen_label) %>%
      dplyr::select(-"chosen_label")

  }

  #check if kinase labels should be colored based on group.
  if (color_kinase_labels_groups == TRUE) {
    p <- p + ggtree::geom_tiplab(ggtree::aes(color = .data$Kinase_Group), size = label_size)#, family = input$chosenFont) # Font needs to be redone
  } else {
    p <- p + ggtree::geom_tiplab(size = label_size, color = default_label_color)#, family = input$chosenFont)
  }

  #check if branches should be highlighted based on group.
  if (!rlang::is_null(highlight_groups)) {
    if (highlight_groups == TRUE) {
      # This produces message
      # ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
      # wrap in purrr::quietly
      # p <- p + ggtree::geom_highlight(data = mrcas, ggplot2::aes(node = node, fill = glabel2), alpha = group_highlighter_alpha)

      p <- quiet_helper_highlight_circular(p = p, mrcas = mrcas, group_highlighter_alpha = group_highlighter_alpha)
    }
  }

  #add interactivity if chosen
  #if(input$staticInteractive == "interactive"){

  #Interactivity removed

  #  #check if branches should be colored based on group.
  #  if (input$colorBranchesGroups == T) {
  #    p <- p + aes(colour=glabel)+
  #      geom_point_interactive(aes(tooltip = label2, data_id = label2), alpha = 0)
  #  } else {

  #    p <- p + geom_point_interactive(aes(tooltip = label2, data_id = label2), alpha = 0)

  #  }

  #} else {
    #check if branches should be colored based on group.
  if (color_branches_groups == TRUE) {
    p <- p + ggtree::aes(colour = .data$glabel)
  }
  #}

  #change color palette
  if (color_palette != "Default ggplot2") {
    if (color_palette == "Custom") {
      p <- p + ggtree::scale_fill_manual(values = custom_color_pal, aesthetics = c("colour", "fill"), na.translate = F)
    } else {
      p <- p + scico::scale_color_scico_d(palette = color_palette, aesthetics = c("colour", "fill"), na.translate = F)
    }
  } else {
    p <- p + ggplot2::scale_color_discrete(aesthetics = c("colour", "fill"), na.translate = F)
  }

  if (!is.null(group_label_radius)) {
    group_labels <- p$data %>%
      dplyr::select("label2", "x", "y", "angle", "id") %>%
      dplyr::mutate(x = .data$x * 4.5 * .env$group_label_radius) %>%
      #dplyr::filter(grepl("^Group [A-Za-z0-9]{1,}$", .data$label2)) %>%
      dplyr::filter(.data$id == "Group") %>%
      dplyr::mutate(label = stringr::str_remove_all(.data$label2, "^Group ")) %>%
      dplyr::select(-"id")


    if (show_group_labels == TRUE) {
      p <- p + ggtree::geom_text(data = group_labels, ggtree::aes(x = .data$x, y = .data$y, color = .data$label, label = .data$label), size = group_label_size)#, family = input$chosenFont) #Add font later
    }
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

  #interactive mode can't handle other fonts in legend for some reason...
  #if (input$staticInteractive == "static") {
  #add font later
    p <- p + ggtree::theme(legend.text = ggplot2::element_text(size = legend_label_size),# family = input$chosenFont),
                   legend.title = ggplot2::element_text(size = legend_title_size))#, family = input$chosenFont))
  #}

  #theme(legend.text = element_text(size = input$legendLabelSize, family = input$chosenFont),
  #      legend.title = element_text(size = input$legendTitleSize, family = input$chosenFont))

  if (hide_legend == TRUE) {
    p <- p + ggtree::theme(legend.position = "none")
  }

  p
}


assign_branch_groups_circular <- function(kinome_df, circular_base) {
  #assign branch group for circular plot

  tmp_df1 <- kinome_df %>%
    dplyr::select("Kinase_Group") %>%
    dplyr::transmute(glabel = paste("Group ", .data$Kinase_Group, sep = ""),
                     group = .data$Kinase_Group) %>%
    dplyr::distinct()

  tmp_df2 <- kinome_df %>%
    dplyr::select("Kinase_Group", "Kinase_Family") %>%
    dplyr::transmute(glabel = paste("Family ", .data$Kinase_Family, sep = ""),
                     group = .data$Kinase_Group) %>%
    dplyr::distinct()

  tmp_df3 <- kinome_df %>%
    dplyr::select("Kinase_Group", "Kinase_Subfamily") %>%
    dplyr::transmute(glabel = paste("Subfamily ", .data$Kinase_Subfamily, sep = ""),
                     group = .data$Kinase_Group) %>%
    dplyr::distinct()

  #add groups of kinases (last branch connecting to kinase)

  tmp_df4 <- circular_base@extraInfo %>%
    dplyr::mutate(glabel = .data$Kinase_Group) %>%
    dplyr::select("node", "glabel") %>%
    dplyr::filter(!is.na(.data$glabel))

  groups2branches <- rbind(tmp_df1,
                           tmp_df2,
                           tmp_df3)


  tibble::tibble(node = (length(circular_base@phylo[["tip.label"]]) + 1):(length(circular_base@phylo[["tip.label"]]) + length(circular_base@phylo[["node.label"]])),
                 value = circular_base@phylo[["node.label"]]) %>%
    dplyr::left_join(groups2branches, by = dplyr::join_by("value" == "glabel")) %>%
    dplyr::select("node", "group") %>%
    dplyr::transmute(node = .data$node,
                     glabel = .data$group) %>%
    rbind(tmp_df4)
}

get_mrcas <- function(kinome_df, circular_base){
  kgroups <- unique(kinome_df$Kinase_Group)

  node <- numeric(length = length(kgroups))
  glabel <- character(length = length(kgroups))

  for(i in 1:length(kgroups)) {
    curr_mrca <- kinome_df %>%
      dplyr::filter(.data$Kinase_Group == .env$kgroups[[i]]) %>%
      dplyr::pull("Kinase_Family")

    curr_mrca <- paste("Family ", curr_mrca, sep = "")

    curr_node <- ggtree::MRCA(circular_base, curr_mrca)
    node[[i]] <- curr_node
  }
  tibble::tibble(node = node, glabel2 = kgroups)
}


#' custom_color_nums_to_pal
#'
#' @param custom_color_nums A reactive created with the function `kinase_groups_to_custom_color_numbers()`
#'
#' @description Convert custom color numbers to a custom palette
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
custom_color_nums_to_pal <- function(custom_color_nums, input) {
  cols <- purrr::map_chr(custom_color_nums, ~ input[[.x]] %||% "")
  # convert empty inputs to transparent
  cols[cols == ""] <- NA

  cols
}


helper_highlight_circular <- function(p, mrcas, group_highlighter_alpha) {
  # .data pronoun somehow does not work inside ggtree::geom_highlight
  # therefore this hack is used
  node <- NULL
  glabel2 <- NULL
  p + ggtree::geom_highlight(data = mrcas, ggplot2::aes(node = node, fill = glabel2), alpha = group_highlighter_alpha)
}

quiet_helper_highlight_circular <- function(...) {
  res <- purrr::quietly(helper_highlight_circular)(...)

  # message
  # ! # Invaild edge matrix for <phylo>. A <tbl_df> is returned.
  # is of no value and is therefore silenced
  # warnings are retained
  if (rlang::is_empty(res$warnings) == FALSE) {
    rlang::warn(res$warnings)
  }

  # if (rlang::is_empty(res$messages) == FALSE) {
    # rlang::inform(res$messages)
  # }

  res$result
}
