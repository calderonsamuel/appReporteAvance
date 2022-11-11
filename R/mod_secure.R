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
            text = "Tareas",
            tabName = "board",
            icon = icon("tasks"),
            selected = TRUE
          )
        )
      ),
      bs4Dash::dashboardBody(
          golem_add_external_resources(),
        bs4Dash::tabItems(
          bs4Dash::tabItem(
            tabName = "board",
            mod_board_ui(ns("board_1"))
          )
        )
      )
    )
  )
}

#' secure Server Functions
#'
#' @noRd
mod_secure_server <- function(id, AppData){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    mod_board_server("board_1", AppData)

  })
}

mod_secure_apptest <-
    function(user_iniciado = "dgco93@mininter.gob.pe") {
        
        AppData <- AppData$new(user_iniciado)
        
        ui <- mod_secure_ui(id = "test")
        
        server <- function(input, output, session) {
            mod_secure_server(id = "test", AppData)
        }
        
        shinyApp(ui, server)
    }

## To be copied in the UI
# mod_secure_ui("secure_1")

## To be copied in the server
# mod_secure_server("secure_1")
