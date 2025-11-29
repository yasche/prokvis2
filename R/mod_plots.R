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
              shiny::uiOutput(ns("custom_color_pal"))
            ),
            open = FALSE
          )
        },

        if (plot_tab == "circular") {
          list(
            "the sidebar is circular",
            shiny::tags$br()
          )
        },
        if (plot_tab == "network") {
          "the sidebar is network"
        },
        if (plot_tab == "circular") {
          "more circular"
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

    output$custom_color_pal <- shiny::renderUI({
      if (input$color_palette == "Custom") {
        "the palette is custom"
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
