#' secure UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_secure_ui <- function(id, privileges){
  ns <- NS(id)
  tagList(
    bs4Dash::dashboardPage(
      bs4Dash::dashboardHeader(title = "Reporte"),
      bs4Dash::dashboardSidebar(
        collapsed = TRUE,
        expandOnHover = FALSE,
        bs4Dash::sidebarMenu(
          id = "sidebar",
          bs4Dash::menuItem(
            text = "Reporte de avance",
            tabName = "progress",
            icon = icon("tasks"),
            selected = TRUE
          ),
          if (privileges != "user1") bs4Dash::menuItem("Asignar tareas", tabName = "tasks", icon = icon("calendar-plus")),
          mod_admin_menuItem(ns("admin_1"), privileges = privileges) # returns a bs4Dash::menuItem
        )
      ),
      bs4Dash::dashboardBody(
        bs4Dash::tabItems(
          bs4Dash::tabItem(
            tabName = "progress",
            mod_progress_ui(ns("progress_1"))
          ),
          bs4Dash::tabItem(
            tabName = "tasks",
            mod_tasks_ui(ns("tasks_1"))
          ),
          mod_admin_ui(ns("admin_1")) # returns a bs4Dash::tabItem
        )
      )
    )
  )
}

#' secure Server Functions
#'
#' @noRd
mod_secure_server <- function(id, user_iniciado){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    mod_tasks_server("tasks_1", user_iniciado)
    mod_progress_server("progress_1", user_iniciado)
    mod_admin_server("admin_1")

  })
}

mod_secure_testapp <- function(privileges = "admin") {

  ui <- mod_secure_ui("test", privileges = privileges)

  server <- function(input, output, session) {
    user_iniciado <- reactive("dgco93@mininter.gob.pe")
    mod_secure_server("test", user_iniciado)
  }

  shinyApp(ui, server)
}

## To be copied in the UI
# mod_secure_ui("secure_1")

## To be copied in the server
# mod_secure_server("secure_1")
