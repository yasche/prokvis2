#' name_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_name_map_ui <- function(id) {
  ns <- NS(id)

  bslib::layout_column_wrap(
    bslib::card(
      shiny::selectInput(ns("species_selection"), "Select Species", choices = species_selection_choices(kinome_data)),
      shiny::textAreaInput(ns("kinases_to_map"), "Input (one per row)", height = 200, value = "ABL\nABR\nMAPK"),
      shiny::actionButton(ns("start_mapping"), "Map", width = 100)
    ),

    bslib::card(
      rhandsontable::rHandsontableOutput(ns("mapped_kinases_hot"), height = "100%"),
      shiny::downloadButton(ns("download_namemap"), "Download"))
  )
}

#' name_map Server Functions
#'
#' @noRd
mod_name_map_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns


    reactive_mapped_kinases_hot <- eventReactive(input$start_mapping, {
      name_map_rhot(kinome_data = kinome_data, species_selection = input$species_selection, kinase_names = input$kinases_to_map)
    })

    output$mapped_kinases_hot <- rhandsontable::renderRHandsontable({
      reactive_mapped_kinases_hot()
    })
  })
}

## To be copied in the UI
# mod_name_map_ui("name_map_1")

## To be copied in the server
# mod_name_map_server("name_map_1")
