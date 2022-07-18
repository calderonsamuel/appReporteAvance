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
        # preloader = list(html = tagList(waiter::spin_pixel(), "Cargando ..."), color = "#3c8dbc"),
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
          bs4Dash::menuItem(
              text = "Plantillas",
              icon = icon("book"),
              tabName = "admin_templates"
          ),
          if (privileges != "user1") bs4Dash::menuItem("Asignar tareas", tabName = "tasks", icon = icon("calendar-plus")),
          if (privileges == "admin") {
            tagList(
              bs4Dash::menuItem(
                text = "User admin",
                icon = icon("user-shield"),
                tabName = "admin_users"
              ),
              bs4Dash::menuItem(
                text = "Grupos admin",
                icon = icon("users"),
                tabName = "admin_groups"
              )
            )
          }
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
          bs4Dash::tabItem(
            tabName = "admin_users",
            mod_users_ui(ns("admin_users_1"))
          ),
          bs4Dash::tabItem(
            tabName = "admin_templates",
            mod_templates_ui(ns("admin_templates_1"))
          ),
          bs4Dash::tabItem(
            tabName = "admin_groups",
            mod_groups_ui(ns("admin_groups_1"))
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

    if (is.null(user_iniciado))  {
        message("No se ha definido 'user_iniciado()' en 'mod_secure_server'")
    } else {
        glue::glue("user_iniciado is '{user_iniciado}'")
    }

    mod_progress_server("progress_1", user_iniciado)
    mod_tasks_server("tasks_1", user_iniciado)
    mod_users_server("admin_users_1")
    mod_templates_server("admin_templates_1", user_iniciado)
    mod_groups_server("admin_groups_1", user_iniciado)

  })
}

mod_secure_testapp <- function(id = "test", privileges = "admin") {

  ui <- mod_secure_ui(id, privileges = privileges)

  server <- function(input, output, session) {
    user_iniciado <- "dgco93@mininter.gob.pe"
    mod_secure_server(id, user_iniciado)
  }

  shinyApp(ui, server)
}

## To be copied in the UI
# mod_secure_ui("secure_1")

## To be copied in the server
# mod_secure_server("secure_1")
