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
    bs4Dash::dashboardPage(
      bs4Dash::dashboardHeader(title = "Reporte"),
      bs4Dash::dashboardSidebar(
        bs4Dash::sidebarMenu(id = "test",
          bs4Dash::menuItem("hola")
        )
      ),
      bs4Dash::dashboardBody(

      )
    )

    # fluidPage(
    #   theme = app_my_theme(),
    #   h1("Reporte de avance"),
    #   mod_form_page_ui("form_page_1")
    # )
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
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "appReporteAvance"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
