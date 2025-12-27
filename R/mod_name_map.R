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
  tagList(
 
  )
}
    
#' name_map Server Functions
#'
#' @noRd 
mod_name_map_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_name_map_ui("name_map_1")
    
## To be copied in the server
# mod_name_map_server("name_map_1")
