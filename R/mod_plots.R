#' plots UI Function
#'
#' @description A shiny Module.
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
              shiny::numericInput(ns("branch_thickness"), "Branch thickness", min = 0, value = 0.15, step = 0.05),
              shiny::checkboxInput(ns("hideLegend"), "Hide legend", value = FALSE),
              shiny::numericInput(ns("plot_height"), "Plot height in pixels", value = 1000),
              shiny::numericInput(ns("plot_width"), "Plot width in pixels", value = 1000),

              if (plot_tab == "phylo") {
                list(
                  shiny::selectInput(ns("phylo_tree_layout"), "Tree layout", choices = c("fan", "circular", "radial", "equal_angle", "daylight", "ape")),
                  shiny::selectInput(ns("phylo_branch_scaling"), "Scale branch length", choices = c("None", "log10", "True length")),
                  shiny::sliderInput(ns("phylo_atypical_length"), "Atypical kinase plot length (inverse)", min = 0.1, max = 2, step = 0.05, value = 0.8),
                  shiny::checkboxInput(ns("adjust_legend_pos_man"), "Adjust legend position manually", value = FALSE),
                  shiny::uiOutput(ns("ui_adjust_legend_pos"))
                )
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
              shiny::selectInput(ns("chosen_font"), "Font", c("some", "font", "names"), multiple = FALSE),
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
                  shiny::numericInput(ns("nudge_kinase_label_X"), "Nudge kinase label X", value = 0, step = 0.001),
                  shiny::numericInput(ns("nudge_kinase_label_Y"), "Nudge kinase label Y", value = 0, step = 0.001)
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
            open = FALSE
          )
        }
      ),

      # the page content goes here
      if (plot_tab == "circular") {
        #"the plot is circular",
        shiny::plotOutput(ns("plot_circular"))
      },
      if (plot_tab == "network") {
        "the plot is network"
      },
      if (plot_tab == "table") {
        DT::dataTableOutput(ns("kinome_table"))
      }

    )

    #if (plot_tab == "circular") {
    #  bslib::layout_sidebar(sidebar = bslib::sidebar("sb_circular"),
    #                        "circular")
    #} else if (plot_tab == "network") {
    #  bslib::layout_sidebar(sidebar = bslib::sidebar("sb_network"),
    #                        "network")
    #}

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

    reactive_plot_circular_mod <- shiny::reactive({
      p <- ggtree::ggtree(reactive_plot_circular_base())

      # access custom color palette
      cols <- purrr::map_chr(reactive_custom_color_nums(), ~ input[[.x]] %||% "")
      # convert empty inputs to transparent
      cols[cols == ""] <- NA

      print(cols)

      # access custom x/y nudge for group labels in network plot
      pos_nudge <- purrr::map_dbl(reactive_custom_xy(), ~ input[[.x]] %||% 0) %>%
        matrix(nrow = length(.) / 2, ncol = 2, byrow = TRUE)

      colnames(pos_nudge) <- c("x_nudge", "y_nudge")

      pos_nudge <- tibble::as_tibble(pos_nudge) %>%
        dplyr::mutate(label = reactive_kinase_groups())

      print(pos_nudge)

      print(input)

      p
    })

    output$plot_circular <- shiny::renderPlot({
      reactive_plot_circular_mod()
    })
    # end code for the circular plot

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
        title = "Edit kinase edges",
        rhandsontable::rHandsontableOutput(ns("group_nodes_hot")),
        size = "xl",
        easyClose = TRUE,
        fade = TRUE
      ))
    })

    shiny::observeEvent(input$edit_families_action_button, {
      shiny::showModal(shiny::modalDialog(
        title = "Edit kinase edges",
        rhandsontable::rHandsontableOutput(ns("family_nodes_hot")),
        size = "xl",
        easyClose = TRUE,
        fade = TRUE
      ))
    })

    shiny::observeEvent(input$edit_subfamilies_action_button, {
      shiny::showModal(shiny::modalDialog(
        title = "Edit kinase edges",
        rhandsontable::rHandsontableOutput(ns("subfamily_nodes_hot")),
        size = "xl",
        easyClose = TRUE,
        fade = TRUE
      ))
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

    output$ui_manual_selection_input <- shiny::renderUI({
      shiny::conditionalPanel(
        condition = "input.show_kinases_labels == 'Manual selection'",
        # Server code missing!
        "The selection is set to Manual",
        ns = ns
      )
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

    ## start code for custom group label position in network plot
    reactive_custom_xy <- shiny::reactive({
      kinase_groups_to_custom_xy(reactive_kinase_groups())
    })

    output$ui_np_group_label_position <- shiny::renderUI({
      shiny::conditionalPanel(
        condition = "input.show_group_labels == true & input.adjust_np_pos_manually == true",
        # Server code is still missing!
        manual_group_label_pos_input(
          custom_xy = reactive_custom_xy(),
          kinase_groups = reactive_kinase_groups(),
          id = id,
          ns = ns
        ),
        ns = ns
      )
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
  })
}

## To be copied in the UI
# mod_plots_ui("plots_1")

## To be copied in the server
# mod_plots_server("plots_1")
