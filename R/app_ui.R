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

    firebase::reqSignin(
      uiOutput("my_ui")
      # h3("hello")
      # mod_secure_ui("secure_1")
    )

    # bs4Dash::dashboardPage(
    #   bs4Dash::dashboardHeader(title = "Reporte"),
    #   bs4Dash::dashboardSidebar(
    #     expandOnHover = FALSE,
    #     bs4Dash::sidebarMenu(id = "test",
    #       bs4Dash::menuItem("Asignar tareas", tabName = "tasks", icon = icon("calendar-plus")),
    #       bs4Dash::menuItem("Reporte de avance", tabName = "progress", icon = icon("tasks")),
    #       bs4Dash::menuItem("Admin", tabName = "admin", icon = icon("user-shield"))
    #     )
    #   ),
    #   bs4Dash::dashboardBody(
    #     bs4Dash::tabItems(
    #       bs4Dash::tabItem(
    #         tabName = "tasks",
    #         mod_tasks_ui("tasks_1")
    #       ),
    #       bs4Dash::tabItem(
    #         tabName = "progress",
    #         mod_progress_ui("progress_1")
    #       ),
    #       bs4Dash::tabItem(
    #         tabName = "admin",
    #         mod_admin_ui("admin_1")
    #       )
    #     )
    #   )
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
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyWidgets::useSweetAlert(),
    firebase::useFirebase(),
    firebase::firebaseUIContainer()
  )
}
