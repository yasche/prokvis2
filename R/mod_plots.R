#' plots UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plots_ui <- function(id, plot_tab) {
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
                  shiny::actionButton(ns("edit_sub_families_action_button"), "Edit subfamily nodes", width = "100%")
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
        textOutput(ns("test_spec_selected"))
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
mod_plots_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$ui_custom_color_pal <- shiny::renderUI({
      if (input$color_palette == "Custom") {
        "the palette is custom"
      }
    })

    output$ui_default_branch_color <- shiny::renderUI({
      if(input$color_branches_groups == FALSE) {
        colourpicker::colourInput(ns("default_branch_color"), "Default branch color", value = "grey")
      }
    })

    output$ui_adjust_legend_pos <- shiny::renderUI({
      if (input$adjust_legend_pos_man == TRUE) {
        list(
          shiny::HTML("Legend position"),
          shiny::numericInput(ns("phylo_legend_x"), "x", min = 0, value = 1, step = 0.05),
          shiny::numericInput(ns("phylo_legend_y"), "y", min = 0, value = 0.5, step = 0.05),
          shiny::sliderInput(ns("phylo_atypical_length"), "Atypical kinase plot length (inverse)", min = 0.1, max = 2, step = 0.05, value = 0.8)
        )
      }
    })

    output$ui_default_label_color <- shiny::renderUI({
      if (input$color_kinase_labels_groups == FALSE) {
        colourpicker::colourInput(ns("default_label_color"), "Default label color", value = "grey")
      }
    })

    output$ui_manual_selection_input <- shiny::renderUI({
      if (input$show_kinases_labels == "Manual selection") {
        # Server code is still missing!
        "The selection is set to Manual"
      }
    })

    output$ui_group_label_size <- shiny::renderUI({
      if (input$show_group_labels == TRUE) {
        shiny::numericInput(ns("group_label_size"), "Group label size", value = 5, step = 0.5)
      }
    })

    output$ui_group_label_radius <- shiny::renderUI({
      if (input$show_group_labels == TRUE) {
        shiny::numericInput(ns("group_label_radius"), "Group label radius", value = 1, step = 0.1)
      }
    })

    output$ui_adjust_np_pos_manually <- shiny::renderUI({
      if (input$show_group_labels == TRUE) {
        shiny::checkboxInput(ns("adjust_np_pos_manually"), "Adjust the label position for each group manually", value = FALSE)
      }
    })

    output$ui_np_group_label_position <- shiny::renderUI({
      if (!is.null(input$adjust_np_pos_manually)) {
        if (input$show_group_labels == TRUE & input$adjust_np_pos_manually == TRUE) {
          # Server code is still missing!
          "np group label positions are set to manual"
        }
      }
    })

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
