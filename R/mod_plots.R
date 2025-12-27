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

    output$test_spec_selected <- renderText({
      input$species_selection
    })
  })
}

## To be copied in the UI
# mod_plots_ui("plots_1")

## To be copied in the server
# mod_plots_server("plots_1")
