#' Run the Shiny Application
#'
#' @description
#' Calling this function launches the prokvis2 Shiny app to create visualizations of kinase data.
#'
#' @param custom_kinome_data Either `NULL` to run the app with the internal `kinome_data` object or the path to a custom `kinome_data.rds` file.
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#'
#' @examples
#' \dontrun{
#' # Start the app with the internal `kinome_data` object:
#' run_app()
#'
#' # Start the app with a user-supplied `kinome_data` file:
#' run_app(custom_kinome_data = "path/to/kinome_data.rds")
#' }
#'
run_app <- function(
  custom_kinome_data = NULL,
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(
      custom_kinome_data = custom_kinome_data
    )
  )
}
