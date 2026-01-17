#' plots UI Function
#'
#' @description Module for the Plots tab. Depending on the sub tab this module's output changes.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plots_ui <- function(id, plot_tab, kinome_data) {
  ns <- NS(id)
  tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        # the page sidebar goes here
        shiny::selectInput(ns("species_selection"), "Species", species_selection_choices(kinome_data)),
        if (plot_tab != "table") {
          bslib::accordion(
            bslib::accordion_panel(
              "Gerneral Appearance", icon = bsicons::bs_icon("palette"),
              shiny::selectInput(ns("color_palette"), "Color palette", c("Default ggplot2", "Custom", scico::scico_palette_names())),
              shiny::uiOutput(ns("ui_custom_color_pal")),
              shiny::checkboxInput(ns("color_branches_groups"), "Color branches based on Groups", value = TRUE),
              shiny::uiOutput(ns("ui_default_branch_color")),
              if (plot_tab == "circular" | plot_tab == "network") {
                shiny::uiOutput(ns("ui_plot_alpha"))
              },
              shiny::numericInput(ns("branch_thickness"), "Branch thickness", min = 0, value = 0.15, step = 0.05),
              shiny::checkboxInput(ns("hide_legend"), "Hide legend", value = FALSE),
              shiny::numericInput(ns("plot_height"), "Plot height in pixels", value = 1000, min = 1, max = 1000),
              shiny::numericInput(ns("plot_width"), "Plot width in pixels", value = 1000, min = 1, max = 1000),

              if (plot_tab == "phylo") {
                list(
                  shiny::selectInput(ns("phylo_tree_layout"), "Tree layout", choices = c("fan", "circular", "radial", "equal_angle", "daylight", "ape")),
                  shiny::selectInput(ns("phylo_branch_scaling"), "Scale branch length", choices = c("None", "log10", "True length")),
                  shiny::sliderInput(ns("phylo_atypical_length"), "Atypical kinase plot length (inverse)", min = 0.1, max = 2, step = 0.05, value = 0.8),
                  shiny::checkboxInput(ns("adjust_legend_pos_man"), "Adjust legend position manually", value = FALSE),
                  shiny::uiOutput(ns("ui_adjust_legend_pos"))
                )
              },
              if (plot_tab == "network") {
                shiny::numericInput(ns("set_seed"), "Plot layout", value = 1, min = 1)
              }
            ),
            bslib::accordion_panel(
              "Node & Edge Settings", icon = bsicons::bs_icon("diagram-3"),
              shiny::checkboxInput(ns("color_kinase_edges_groups"), "Color nodes and edges based on groups"),
              shiny::actionButton(ns("edit_kinases_action_button"), "Edit kinase edges", width = "100%"),
              if (plot_tab != "phylo") {
                list(
                  shiny::tags$br(),
                  shiny::actionButton(ns("edit_groups_action_button"), "Edit group nodes", width = "100%"),
                  shiny::tags$br(),
                  shiny::actionButton(ns("edit_families_action_button"), "Edit family nodes", width = "100%"),
                  shiny::tags$br(),
                  shiny::actionButton(ns("edit_subfamilies_action_button"), "Edit subfamily nodes", width = "100%")
                )
              }
            ),
            bslib::accordion_panel(
              "Label Settings", icon = bsicons::bs_icon("fonts"),
              #shiny::selectInput(ns("chosen_font"), "Font", c("some", "font", "names"), multiple = FALSE),
              shiny::selectInput(ns("show_kinases_labels"), "Show kinase labels",
                                 choices = c("None",
                                             "All",
                                             "Annotated",
                                             "Manual selection"),
                                 selected = "All"),

              shiny::uiOutput(ns("ui_manual_selection_input")),

              shiny::selectInput(ns("show_which_kinase_labels"),
                                 list("Label source",
                                      bslib::tooltip(
                                          bsicons::bs_icon("question-circle"),
                                          "A custom label (Clabel) for each kinase, group, family and subfamily can be provided in the Node & Edge Settings.",
                                          placement = "right"
                                        )
                                      ),
                                 choices = c("Manning name",
                                             "Uniprot gene name",
                                             "Uniprot entry",
                                             "Uniprot accession",
                                             "Custom")),
              shiny::checkboxInput(ns("color_kinase_labels_groups"), "Color kinase labels based on Groups", value = TRUE),

              shiny::uiOutput(ns("ui_default_label_color")),

              shiny::numericInput(ns("label_size"), "Kinase label size", min = 0, value = 1.5, step = 0.1),
              shiny::numericInput(ns("legend_title_size"), "Legend title size", min = 0, value = 18, step = 1),
              shiny::numericInput(ns("legend_label_size"), "Legend text size", min = 0, value = 15, step = 1),

              if (plot_tab == "network") {
                list(
                  shiny::numericInput(ns("nudge_kinase_label_x"), "Nudge kinase label X", value = 0, step = 0.001),
                  shiny::numericInput(ns("nudge_kinase_label_y"), "Nudge kinase label Y", value = 0, step = 0.001)
                )
              },

              if (plot_tab != "phylo") {
                list(
                  shiny::checkboxInput(ns("show_group_labels"), "Show group labels", value = TRUE),
                  shiny::uiOutput(ns("ui_group_label_size"))
                )
              },

              if (plot_tab == "circular") {
                shiny::uiOutput(ns("ui_group_label_radius"))
              },

              if (plot_tab == "network") {
                list(
                  shiny::uiOutput(ns("ui_adjust_np_pos_manually")),
                  shiny::uiOutput(ns("ui_np_group_label_position"))
                )
              }
            ),
            bslib::accordion_panel(
              "Save Plot", icon = bsicons::bs_icon("floppy"),
              shiny::textInput(ns("download_plot_title"), label = "Add a plot title (optional)"),
              shiny::numericInput(ns("download_plot_width"), label = "Plot width", value = 1000, min = 1, max = 1000),
              shiny::numericInput(ns("download_plot_height"), label = "Plot height", value = 1000, min = 1, max = 1000),
              shiny::textInput(ns("download_plot_filename"), label = "File name", value = "plot"),
              shiny::selectInput(ns("download_plot_extension"), label = "File type", choices = c("PNG" = "png", "SVG" = "svg", "ggplot object (RDS)" = "rds"), multiple = FALSE, selected = "PNG"),
              if (plot_tab == "circular") {
                shiny::downloadButton(ns("plot_download_circular"), label = "Download")
              },
              if (plot_tab == "network") {
                shiny::downloadButton(ns("plot_download_network"), label = "Download")
              }
            ),
            open = FALSE
          )
        }
      ),

      # the page content goes here
      if (plot_tab == "circular") {
        shiny::plotOutput(ns("plot_circular"), width = 1000)
      },
      if (plot_tab == "network") {
        shiny::plotOutput(ns("plot_network"), width = 1000)
      },
      if (plot_tab == "table") {
        DT::dataTableOutput(ns("kinome_table"))
      }

    )
  )
}

#' plots Server Functions
#'
#' @noRd
mod_plots_server <- function(id, kinome_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    reactive_kinome_df <- shiny::reactive({
      extract_kinome_df(kinome_data, input$species_selection)
    })


    # start code for the circular plot
    reactive_plot_circular_base <- shiny::reactive({
      plot_circular_base(reactive_kinome_df())
    })

    reactive_mrcas <- shiny::reactive({
      get_mrcas(reactive_kinome_df(), reactive_plot_circular_base())
    })

    reactive_custom_color_pal <- shiny::reactive({
      custom_color_nums_to_pal(reactive_custom_color_nums(), input = input)
    })

    reactive_plot_circular_mod <- shiny::reactive({
      p <- plot_circular_edited(
        circular_base = reactive_plot_circular_base(),
        combined_nodes_and_edges = reactive_combined_nodes_and_edges(),
        selected_kinome = reactive_kinome_df(),
        color_branches_groups = input$color_branches_groups,
        branch_thickness = input$branch_thickness,
        default_branch_color = input$default_branch_color,
        color_kinase_edges_groups = input$color_kinase_edges_groups,
        show_kinases_labels = input$show_kinases_labels,
        kinase_labels_manual_selection = input$kinase_labels_manual_selection,
        kinase_edges_hot = input$kinase_edges_hot,
        show_which_kinase_labels = input$show_which_kinase_labels,
        color_kinase_labels_groups = input$color_kinase_labels_groups,
        label_size = input$label_size,
        default_label_color = input$default_label_color,
        mrcas = reactive_mrcas(),
        color_palette = input$color_palette,
        custom_color_pal = reactive_custom_color_pal(),
        group_label_radius = input$group_label_radius,
        show_group_labels = input$show_group_labels,
        group_label_size = input$group_label_size,
        legend_label_size = input$legend_label_size,
        legend_title_size = input$legend_title_size,
        hide_legend = input$hide_legend,
        highlight_groups = input$highlight_groups,
        group_highlighter_alpha = input$group_highlighter_alpha
      )

      ## access custom color palette
      #cols <- purrr::map_chr(reactive_custom_color_nums(), ~ input[[.x]] %||% "")
      ## convert empty inputs to transparent
      #cols[cols == ""] <- NA

      #print(cols)

      # access custom x/y nudge for group labels in network plot
      #pos_nudge <- purrr::map_dbl(reactive_custom_xy(), ~ input[[.x]] %||% 0)
      #pos_nudge <- matrix(nrow = length(pos_nudge) / 2, ncol = 2, byrow = TRUE)

      #colnames(pos_nudge) <- c("x_nudge", "y_nudge")

      #pos_nudge <- tibble::as_tibble(pos_nudge) %>%
      #  dplyr::mutate(label = reactive_kinase_groups())

      #print(pos_nudge)

      #print(input)

      p
    })



    output$plot_circular <- shiny::renderPlot({
      reactive_plot_circular_mod()
    }, width = function() input$plot_width, height = function() input$plot_height, execOnResize = TRUE)
    # end code for the circular plot

    # start code for the network plot
    reactive_plot_network_base <- shiny::reactive({
      plot_network_base(reactive_kinome_df(), set_seed = input$set_seed)
    })

    reactive_plot_network_mod <- shiny::reactive({
      plot_network_edited(network_base = reactive_plot_network_base(),
                          combined_nodes_and_edges = reactive_combined_nodes_and_edges(),
                          selected_kinome = reactive_kinome_df(),
                          color_branches_groups = input$color_branches_groups,
                          branch_thickness = input$branch_thickness,
                          default_branch_color = input$default_branch_color,
                          color_kinase_edges_groups = input$color_kinase_edges_groups,
                          show_kinases_labels = input$show_kinases_labels,
                          kinase_labels_manual_selection = input$kinase_labels_manual_selection,
                          kinase_edges_hot = input$kinase_edges_hot,
                          nudge_kinase_label_x = input$nudge_kinase_label_x,
                          nudge_kinase_label_y = input$nudge_kinase_label_y,
                          show_which_kinase_labels = input$show_which_kinase_labels,
                          color_kinase_labels_groups = input$color_kinase_labels_groups,
                          label_size = input$label_size,
                          default_label_color = input$default_label_color,
                          highlight_groups = input$highlight_groups,
                          group_highlighter_alpha = input$group_highlighter_alpha,
                          color_palette = input$color_palette,
                          custom_color_pal = reactive_custom_color_pal(),
                          custom_xy_nudge = reactive_custom_xy_nudge(),
                          show_group_labels = input$show_group_labels,
                          group_label_size = input$group_label_size,
                          legend_label_size = input$legend_label_size,
                          legend_title_size = input$legend_title_size,
                          hide_legend = input$hide_legend)
    })

    output$plot_network <- shiny::renderPlot({
      reactive_plot_network_mod()
    }, width = function() input$plot_width, height = function() input$plot_height, execOnResize = TRUE)
    # end code for the network plot

    # start code for the manual editing of nodes and edges
    reactive_kinase_edges_rhot <- shiny::reactive({
      nodes_and_edges(reactive_kinome_df(), "Manning_Name")
    })

    reactive_subfamily_nodes_rhot <- shiny::reactive({
      nodes_and_edges(reactive_kinome_df(), "Kinase_Subfamily")
    })

    reactive_family_nodes_rhot <- shiny::reactive({
      nodes_and_edges(reactive_kinome_df(), "Kinase_Family")
    })

    reactive_group_nodes_rhot <- shiny::reactive({
      nodes_and_edges(reactive_kinome_df(), "Kinase_Group")
    })

    output$kinase_edges_hot <- rhandsontable::renderRHandsontable({
      reactive_kinase_edges_rhot()
    })

    output$group_nodes_hot <- rhandsontable::renderRHandsontable({
      reactive_group_nodes_rhot()
    })

    output$family_nodes_hot <- rhandsontable::renderRHandsontable({
      reactive_family_nodes_rhot()
    })

    output$subfamily_nodes_hot <- rhandsontable::renderRHandsontable({
      reactive_subfamily_nodes_rhot()
    })

    shiny::observeEvent(input$edit_kinases_action_button, {
      shiny::showModal(shiny::modalDialog(
        title = "Edit kinase edges",
        list(
          shiny::HTML("<p>
                <b>Name:</b> The Manning name of the kinase. Other identifiers can be mapped to the corresponding Manning name in the Name Mapping tab.<br>
                <b>Size:</b> The size of the point (or other shape). Can be used to represent quantitative data (e.g., IC<sub>50</sub> or <i>K</i><sub>D</sub> values).<br>
                <b>Color:</b> The inner color of the point (or other shape). Can be used to represent quantitative or qualitative data.<br>
                <b>Shape:</b> The shape. Can be used to represent qualitative data.<br>
                <b>Stroke:</b> The color of the border around the point (or other shape).<br>
                <b>Stroke_Width:</b> The width of the border around the point (or other shape).<br>
                <b>Clabel:</b> A custom label. Label source must be set to Custom in the Label Settings to take effect.
             </p>"),
          rhandsontable::rHandsontableOutput(ns("kinase_edges_hot"))
        ),
        size = "xl",
        easyClose = TRUE,
        fade = TRUE
      ))
    })

    shiny::observeEvent(input$edit_groups_action_button, {
      shiny::showModal(shiny::modalDialog(
        title = "Edit group nodes",
        rhandsontable::rHandsontableOutput(ns("group_nodes_hot")),
        size = "xl",
        easyClose = TRUE,
        fade = TRUE
      ))
    })

    shiny::observeEvent(input$edit_families_action_button, {
      shiny::showModal(shiny::modalDialog(
        title = "Edit family nodes",
        rhandsontable::rHandsontableOutput(ns("family_nodes_hot")),
        size = "xl",
        easyClose = TRUE,
        fade = TRUE
      ))
    })

    shiny::observeEvent(input$edit_subfamilies_action_button, {
      shiny::showModal(shiny::modalDialog(
        title = "Edit subfamily nodes",
        rhandsontable::rHandsontableOutput(ns("subfamily_nodes_hot")),
        size = "xl",
        easyClose = TRUE,
        fade = TRUE
      ))
    })

    reactive_combined_nodes_and_edges <- shiny::reactive({
      #print(rhandsontable::hot_to_r(input$group_nodes_hot))
      #print("")
      #print("")
      r_tbl <- combine_nodes_and_edges(kinase_edges_hot = input$kinase_edges_hot,
                              group_nodes_hot = input$group_nodes_hot,
                              family_nodes_hot = input$family_nodes_hot,
                              subfamily_nodes_hot = input$subfamily_nodes_hot,
                              kinome_df = reactive_kinome_df())

      #print(r_tbl, n = 1000)
      r_tbl
    })
    # start code for the manual editing of nodes and edges

    # start code for server-side UI elements
    ## start code for custom group color pal

    #output$ui_custom_color_pal_tf <- shiny::renderUI({
    # switched to conditionalPanel to prevent re-run each time palette is changed
    #  col_ui_element <- list(
    #    #partly adapted from https://mastering-shiny.org/action-dynamic.html
    #    "Choose a custom color for each group",
    #    shiny::HTML("<br><br>"),
    #    purrr::map(ns(reactive_custom_color_nums()), ~ colourpicker::colourInput(.x, reactive_kinase_groups()[as.numeric(stringr::str_remove(.x, paste0(id, "-custom_group_col", collapse = "")))], value = "#BEBEBE"))
    #    #purrr::map(ns(reactive_custom_color_nums()), ~ colourpicker::colourInput(.x, reactive_kinase_groups()[as.numeric(stringr::str_remove(stringr::str_split_i(.x, "\\-", 2), "custom_group_col"))], value = "#BEBEBE"))
    #  )
    #  if (input$color_palette == "Custom") {
    #    col_ui_element
    #  }
    #})

    output$ui_custom_color_pal <- shiny::renderUI({
      shiny::conditionalPanel(
        condition = "input.color_palette == 'Custom'",
        custom_group_color_input(custom_color_nums = reactive_custom_color_nums(), kinase_groups = reactive_kinase_groups(), ns = ns, id = id),
        ns = ns
      )
    })

    reactive_kinase_groups <- shiny::reactive({
      extract_kinase_groups(reactive_kinome_df())
    })

    reactive_custom_color_nums <- shiny::reactive({
      kinase_groups_to_custom_color_numbers(reactive_kinase_groups())
    })
    ## end code for custom group color pal

    output$ui_default_branch_color <- shiny::renderUI({
      shiny::conditionalPanel(
        condition = "input.color_branches_groups == false",
        colourpicker::colourInput(ns("default_branch_color"), "Default branch color", value = "grey"),
        ns = ns
      )
    })

    output$ui_adjust_legend_pos <- shiny::renderUI({
      shiny::conditionalPanel(
        condition = "input.adjust_legend_pos_man == true",
        list(
          shiny::HTML("Legend position"),
          shiny::numericInput(ns("phylo_legend_x"), "x", min = 0, value = 1, step = 0.05),
          shiny::numericInput(ns("phylo_legend_y"), "y", min = 0, value = 0.5, step = 0.05)
        ),
        ns = ns
      )
    })

    output$ui_default_label_color <- shiny::renderUI({
      shiny::conditionalPanel(
        condition = "input.color_kinase_labels_groups == false",
        colourpicker::colourInput(ns("default_label_color"), "Default label color", value = "grey"),
        ns = ns
      )
    })

    reactive_kinases <- shiny::reactive({
      selected_kinome <- reactive_kinome_df()
      selected_kinome$Manning_Name
    })

    output$ui_manual_selection_input <- shiny::renderUI({
      shiny::conditionalPanel(
        condition = "input.show_kinases_labels == 'Manual selection'",
        # Server code missing!
        shiny::selectizeInput(ns("kinase_labels_manual_selection"), "", reactive_kinases(), multiple = TRUE),
        ns = ns
      )
    })

    output$manual_annotation_selection_input <- renderUI({

    })


    output$ui_group_label_size <- shiny::renderUI({
      shiny::conditionalPanel(
        condition = "input.show_group_labels == true",
        shiny::numericInput(ns("group_label_size"), "Group label size", value = 5, step = 0.5),
        ns = ns
      )
    })

    output$ui_group_label_radius <- shiny::renderUI({
      shiny::conditionalPanel(
        condition = "input.show_group_labels == true",
        shiny::numericInput(ns("group_label_radius"), "Group label radius", value = 1, step = 0.1),
        ns = ns
      )
    })

    output$ui_adjust_np_pos_manually <- shiny::renderUI({
      shiny::conditionalPanel(
        condition = "input.show_group_labels == true",
        shiny::checkboxInput(ns("adjust_np_pos_manually"), "Adjust the label position for each group manually", value = FALSE),
        ns = ns
      )
    })

    output$ui_plot_alpha <- shiny::renderUI({
      list(
        shiny::checkboxInput(ns("highlight_groups"), 'Highlight Groups', value = FALSE),
        shiny::conditionalPanel(
          condition = "input.highlight_groups == true",
          shiny::sliderInput(ns("group_highlighter_alpha"), "Alpha", min = 0, max = 1, value = 0.1, step = 0.01),
          ns = ns
        )
      )
    })

    ## start code for custom group label position in network plot
    reactive_custom_xy <- shiny::reactive({
      kinase_groups_to_custom_xy(reactive_kinase_groups())
    })

    output$ui_np_group_label_position <- shiny::renderUI({
      shiny::conditionalPanel(
        condition = "input.show_group_labels == true & input.adjust_np_pos_manually == true",
        manual_group_label_pos_input(
          custom_xy = reactive_custom_xy(),
          kinase_groups = reactive_kinase_groups(),
          id = id,
          ns = ns
        ),
        ns = ns
      )
    })

    reactive_custom_xy_nudge <- shiny::reactive({
      custom_xy_nums_to_nudge(reactive_custom_xy(), input, reactive_kinase_groups())
    })
    ## end code for custom group label position in network plot
    # end code for server-side UI elements

    output$test_spec_selected <- renderText({
      input$species_selection
    })

    output$kinome_table <- DT::renderDataTable({
      DT::datatable(
        extract_kinome_df(kinome_data = kinome_data, species_selection = input$species_selection),
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          pageLength = -1,
          buttons = list(
            extend = 'collection',
            buttons = c('copy', 'csv', 'excel', 'pdf'),
            text = 'Download'
          )
        )
      )
    })

    output$plot_download_circular <- shiny::downloadHandler(
      filename = function(filename = input$download_plot_filename,
                          extension = input$download_plot_extension) {
        paste0(filename, ".", extension)
      },
      content = function(file) {
        p = reactive_plot_circular_mod()

        p_width <- input$download_plot_width
        p_height <- input$download_plot_height

        p_width_inch <- p_width / 72
        p_height_inch <- p_height / 72

        if (input$download_plot_extension == "svg") {
          svg(file, width = p_width_inch, height = p_height_inch)
          plot(p + ggplot2::labs(title = input$download_plot_title))
          dev.off()
        } else if (input$download_plot_extension == "png") {
          png(file, width = p_width_inch, height = p_height_inch, units = "in", res = 600)
          plot(p + ggplot2::labs(title = input$download_plot_title))
          dev.off()
        } else if (input$download_plot_extension == "rds") {
          readr::write_rds(p, file = file)
        }
      }
    )

    output$plot_download_network <- shiny::downloadHandler(
      filename = function(filename = input$download_plot_filename,
                          extension = input$download_plot_extension) {
        paste0(filename, ".", extension)
      },
      content = function(file) {
        p = reactive_plot_network_mod()

        p_width <- input$download_plot_width
        p_height <- input$download_plot_height

        p_width_inch <- p_width / 72
        p_height_inch <- p_height / 72

        if (input$download_plot_extension == "svg") {
          svg(file, width = p_width_inch, height = p_height_inch)
          plot(p + ggplot2::labs(title = input$download_plot_title))
          dev.off()
        } else if (input$download_plot_extension == "png") {
          png(file, width = p_width_inch, height = p_height_inch, units = "in", res = 600)
          plot(p + ggplot2::labs(title = input$download_plot_title))
          dev.off()
        } else if (input$download_plot_extension == "rds") {
          readr::write_rds(p, file = file)
        }
      }
    )
  })
}

## To be copied in the UI
# mod_plots_ui("plots_1")

## To be copied in the server
# mod_plots_server("plots_1")
