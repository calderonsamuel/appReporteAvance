#' secure UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_secure_ui <- function(id, AppData){
  ns <- NS(id)
  
  tagList(
    bs4Dash::dashboardPage(
        freshTheme = custom_theme(),
        dark = NULL,
        # preloader = list(html = tagList(waiter::spin_pixel(), "Cargando ..."), color = "#3c8dbc"),
      header = bs4Dash::dashboardHeader(
          title = "Reporte", 
          rightUi = tags$li(class = "nav-item dropdown", 
            tags$a("Ver manual", class = "nav-link", href="https://manualreportes.productosdedatos.com", target="_blank")
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
          bs4Dash::controlbarMenu(
              bs4Dash::controlbarItem(
                  title = "Configuración",
                  mod_config_ui(ns("config_1"), AppData)
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
    
    config <- mod_config_server("config_1", AppData, reactive(input$controlbar))
    
    mod_board_server("board_1", AppData, config)

  })
}

mod_secure_apptest <-
    function(user_iniciado = Sys.getenv("REPORTES_EMAIL")) {
        
        AppData <- AppData$new(user_iniciado)
        
        ui <- mod_secure_ui(id = "test", AppData)
        
        server <- function(input, output, session) {
            mod_secure_server(id = "test", AppData)
        }
        
        shinyApp(ui, server)
    }

## To be copied in the UI
# mod_secure_ui("secure_1")

## To be copied in the server
# mod_secure_server("secure_1")
