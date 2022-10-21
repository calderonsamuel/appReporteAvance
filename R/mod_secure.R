#' secure UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_secure_ui <- function(id, SessionData){
  ns <- NS(id)
  privileges <- isolate(SessionData$privileges)
  user_iniciado <- isolate(SessionData$user_id)
  
  tagList(
    bs4Dash::dashboardPage(
        freshTheme = custom_theme(),
        # preloader = list(html = tagList(waiter::spin_pixel(), "Cargando ..."), color = "#3c8dbc"),
      bs4Dash::dashboardHeader(
          title = "Reporte" 
      ),
      bs4Dash::dashboardSidebar(
        collapsed = TRUE,
        expandOnHover = FALSE,
        bs4Dash::sidebarMenu(
          id = ns("sidebar"),
          bs4Dash::menuItem(
            text = "Reporte de avance",
            tabName = "progress",
            icon = icon("tasks"),
            selected = TRUE
          ),
          bs4Dash::menuItem("Asignar tareas", tabName = "tasks", icon = icon("calendar-plus")),
          bs4Dash::menuItem(
              text = "Plantillas",
              icon = icon("book"),
              tabName = "templates"
          ),
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
          # shinyWidgets::useSweetAlert(),
          # shinyjs::useShinyjs(),
          golem_add_external_resources(),
        bs4Dash::tabItems(
          bs4Dash::tabItem(
            tabName = "progress",
            mod_progress_ui(ns("progress_1"))
          ),
          bs4Dash::tabItem(
            tabName = "tasks",
            mod_tasks_ui(ns("tasks_1"), SessionData)
          ),
          bs4Dash::tabItem(
            tabName = "admin_users",
            mod_users_ui(ns("admin_users_1"))
          ),
          bs4Dash::tabItem(
            tabName = "templates",
            mod_templates_ui(ns("templates_1"), user_iniciado)
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
mod_secure_server <- function(id, SessionData){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    user_iniciado <- isolate(SessionData$user_id)
    
    privileges <- isolate(SessionData$privileges)
    
    if (is.null(user_iniciado))  {
        message("No se ha definido 'user_iniciado()' en 'mod_secure_server'")
    } else {
        glue::glue("user_iniciado is '{user_iniciado}'")
    }

    mod_progress_server("progress_1", SessionData)
    mod_tasks_server("tasks_1", SessionData)
    mod_templates_server("templates_1", SessionData)

    if (privileges == "admin") {
        mod_users_server("admin_users_1", SessionData)
        mod_groups_server("admin_groups_1", SessionData)
    }

  })
}

mod_secure_apptest <-
    function(user_iniciado = "dgco93@mininter.gob.pe") {
        
        SessionData <- SessionData$new(user_iniciado)
        
        ui <- mod_secure_ui(id = "test", SessionData)
        
        server <- function(input, output, session) {
            mod_secure_server(id = "test", SessionData)
        }
        
        shinyApp(ui, server)
    }

## To be copied in the UI
# mod_secure_ui("secure_1")

## To be copied in the server
# mod_secure_server("secure_1")
