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
        "the plot is circular"
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

  })
}

## To be copied in the UI
# mod_plots_ui("plots_1")

## To be copied in the server
# mod_plots_server("plots_1")
