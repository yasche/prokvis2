#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  if (is.null(golem::get_golem_options("custom_kinome_data"))) {
    print("None provided")
  } else {
    print(golem::get_golem_options("custom_kinome_data"))
  }

  # Your application server logic
  mod_plots_server("plots_ui_c")
  mod_plots_server("plots_ui_n")
  mod_plots_server("plots_ui_pt")
  mod_plots_server("plots_ui_k")
  mod_name_map_server("name_map_1")
}
