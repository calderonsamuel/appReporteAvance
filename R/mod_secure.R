#' secure UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_secure_ui <- function(id, app_data){
  ns <- NS(id)
  
  tagList(
    bs4Dash::dashboardPage(
        freshTheme = custom_theme(),
        dark = NULL,
        # preloader = list(html = tagList(waiter::spin_pixel(), "Cargando ..."), color = "#3c8dbc"),
      header = bs4Dash::dashboardHeader(
          title = "Reporte", 
          rightUi = tags$li(class = "nav-item dropdown", 
            tags$a("Ver manual", class = "nav-link", href="https://calderonsamuel.github.io/manual_de_reportes/", target="_blank")
          ),
          controlbarIcon = fontawesome::fa("fas fa-gear")
      ),
      sidebar = bs4Dash::dashboardSidebar(
        collapsed = TRUE,
        expandOnHover = FALSE,
        bs4Dash::sidebarMenu(
          id = ns("sidebar"),
          bs4Dash::menuItem(
            text = "Tareas",
            tabName = "board",
            icon = icon("tasks"),
            selected = TRUE
          )
        )
      ),
      body = bs4Dash::dashboardBody(
          golem_add_external_resources(),
        bs4Dash::tabItems(
          bs4Dash::tabItem(
            tabName = "board",
            mod_board_ui(ns("board_1"))
          )
        )
      ),
      controlbar = bs4Dash::dashboardControlbar(
          id = ns("controlbar"),
          mod_controlbar_ui(ns("controlbar_1"), app_data)
      )
    )
  )
}

#' secure Server Functions
#'
#' @noRd
mod_secure_server <- function(id, app_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    controlbar <- mod_controlbar_server("controlbar_1", app_data)
    
    mod_board_server("board_1", app_data, controlbar)

  })
}

mod_secure_apptest <-
    function(user_iniciado = Sys.getenv("REPORTES_EMAIL")) {
        
        app_data <- AppData$new(user_iniciado)
        
        ui <- tags$div(
            uiOutput("ui")
        )
        
        server <- function(input, output, session) {
            output$ui <- renderUI(mod_secure_ui(id = "test", app_data))
            
            mod_secure_server(id = "test", app_data)
        }
        
        shinyApp(ui, server)
    }

## To be copied in the UI
# mod_secure_ui("secure_1")

## To be copied in the server
# mod_secure_server("secure_1")
