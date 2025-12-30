#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  if (is.null(golem::get_golem_options("custom_kinome_data"))) {
    use_kinome_data <- prokvis2::kinome_data
  } else {
    # add dedicated read function with sanity check and diagnostic messages later
    use_kinome_data <- readr::read_rds(golem::get_golem_options("custom_kinome_data"))
  }
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      theme = bslib::bs_theme(version = 5, bootswatch = "pulse"),
      #title = shiny::HTML(paste0("<img src = '", paste0(app_sys("app/www"), "/favicon.png"), "' height = 80>")),
      title = "prokvis2",
      window_title = "prokvis2",
      bslib::nav_panel("Plots",
                       bslib::navset_card_underline(
                         bslib::nav_panel("Circular", mod_plots_ui("plots_ui_c", "circular", kinome_data = use_kinome_data)),
                         bslib::nav_panel("Network", mod_plots_ui("plots_ui_n", "network", kinome_data = use_kinome_data)),
                         bslib::nav_panel("Phylogenetic tree", mod_plots_ui("plots_ui_pt", "phylo", kinome_data = use_kinome_data)),
                         bslib::nav_panel("Kinases", mod_plots_ui("plots_ui_k", "table", kinome_data = use_kinome_data)))
                       ),
      bslib::nav_panel("Name Mapping",
                       mod_name_map_ui("name_map_1", kinome_data = use_kinome_data)),
      bslib::nav_panel("Help"),
      bslib::nav_panel("About"),
      bslib::nav_spacer(),
      bslib::nav_item(shiny::tags$a(shiny::icon("github"), "GitHub", href = "https://github.com/yasche", target = "_blank")),
      bslib::nav_item(shiny::tags$a(shiny::icon("envelope"), "Contact", href = "mailto:mail@yannik.science", target = "_blank"))
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
