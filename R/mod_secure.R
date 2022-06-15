#' secure UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_secure_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::dashboardPage(
      bs4Dash::dashboardHeader(title = "Reporte"),
      bs4Dash::dashboardSidebar(
        expandOnHover = FALSE,
        bs4Dash::sidebarMenu(
          id = "test",
          bs4Dash::menuItem("Asignar tareas", tabName = "tasks", icon = icon("calendar-plus")),
          bs4Dash::menuItem("Reporte de avance", tabName = "progress", icon = icon("tasks")),
          bs4Dash::menuItem("Admin", tabName = "admin", icon = icon("user-shield"))
        )
      ),
      bs4Dash::dashboardBody(
        bs4Dash::tabItems(
          bs4Dash::tabItem(
            tabName = "tasks",
            # verbatimTextOutput(ns("user_iniciado")),
            mod_tasks_ui(ns("tasks_1"))
          ),
          bs4Dash::tabItem(
            tabName = "progress",
            mod_progress_ui(ns("progress_1"))
          ),
          bs4Dash::tabItem(
            tabName = "admin",
            mod_admin_ui(ns("admin_1"))
          )
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
    # mod_form_page_server("form_page_1")
    mod_tasks_server("tasks_1", user_iniciado)
    mod_progress_server("progress_1", user_iniciado)
    mod_admin_server("admin_1")

    # output$user_iniciado <- renderPrint(user_iniciado())
  })
}

mod_secure_testapp <- function() {

  ui <- fluidPage(mod_secure_ui("test"))

  server <- function(input, output, session) {
    user_iniciado <- reactive("dgco93")
    mod_secure_server("test", user_iniciado)
  }

  shinyApp(ui, server)
}

## To be copied in the UI
# mod_secure_ui("secure_1")

## To be copied in the server
# mod_secure_server("secure_1")
