#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      theme = bslib::bs_theme(version = 5, bootswatch = "pulse"),
      title = shiny::HTML("<img src = 'resources/prelim_logo.png' height = 80>"),
      window_title = "prokvis2",
      bslib::nav_panel("Plots"),
      bslib::nav_panel("Name Mapping"),
      bslib::nav_panel("Help"),
      bslib::nav_panel("About"),
      bslib::nav_spacer(),
      bslib::nav_item(shiny::tags$a(shiny::icon("github"), "GitHub", href = "https://github.com/yasche", target = "_blank")),
      bslib::nav_item(shiny::tags$a(shiny::icon("envelope"), "Contact", href = "https://posit.co", target = "_blank"))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "prokvis2"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
