#' about UI Function
#'
#' @description Module for the About tab. Only here to display the contents of `about.html`.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_about_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_column_wrap(
    bslib::card(system.file("app", "www", "about.html", package = "prokvis2") %>%
           readr::read_lines() %>%
           paste(collapse = "\n") %>%
           shiny::HTML())
  )
}

#' About server is not needed
#'
#' @description
#' Not really needed (yet)
#'
#' @noRd
mod_about_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_about_ui("about_1")

## To be copied in the server
# mod_about_server("about_1")
