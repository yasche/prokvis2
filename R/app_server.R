#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  if (is.null(golem::get_golem_options("custom_kinome_data"))) {
    use_kinome_data <- prokvis2::kinome_data
  } else {
    # add dedicated read function with sanity check and diagnostic messages later
    use_kinome_data <- readr::read_rds(golem::get_golem_options("custom_kinome_data"))
  }

  # Your application server logic
  mod_plots_server("plots_ui_c", kinome_data = use_kinome_data)
  mod_plots_server("plots_ui_n", kinome_data = use_kinome_data)
  mod_plots_server("plots_ui_pt", kinome_data = use_kinome_data)
  mod_plots_server("plots_ui_k", kinome_data = use_kinome_data)
  mod_name_map_server("name_map_1", kinome_data = use_kinome_data)
}
