#' bslib UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_bslib_ui <- function(id) {
    ns <- NS(id)
    
    config_accordion <- bslib::accordion(
        multiple = FALSE,
        
        bslib::accordion_panel(
            title = "Configuración general",
            icon = fontawesome::fa("people-roof"),
            
            shiny::selectInput(
                inputId = ns("group"),
                label = "Equipo", 
                choices = paste("Equipo Nº", 1:6),
                selected = "a"
            )
        ),
        bslib::accordion_panel(
            title = "Gestionar usuarios",
            icon = fontawesome::fa("user-pen")
        ),
        bslib::accordion_panel(
            title = "Gestionar procesos",
            icon = fontawesome::fa("diagram-project")
        ),
        bslib::accordion_panel(
            title = "Gestionar unidades de medida",
            icon = fontawesome::fa("temperature-low")
        ),
    )
    
    bslib::page_navbar(
        title = "Reportes",
        theme = bslib::bs_theme(version = 5),
        padding = 0,
        
        bslib::nav_panel(
            title = bsicons::bs_icon("kanban"),
            class = "h-100",
            bslib::layout_sidebar(
                padding = 0,
                
                sidebar = bslib::sidebar(
                    position = "right",
                    open = "open",
                    width = 350,
                   
                    config_accordion 
                ), 
                
                tags$div(
                    class = "row",
                    
                    lapply(1:4, \(x) {
                        tags$div(
                            class = "card col-12 col-md-3",
                            "hi"
                        )
                    })
                )
                
                
            )
        )
    )
}

#' bslib Server Functions
#'
#' @noRd
mod_bslib_server <- function(id, app_data) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        roles <- load_roles()
        
        
        
    })
}

## To be copied in the UI
# mod_bslib_ui("bslib_1")

## To be copied in the server
# mod_bslib_server("bslib_1")

mod_bslib_apptest <- function(id = "test") {
    ui <- fluidPage(
        mod_bslib_ui(id)
    )
    
    server <- function(input, output, session) {
        mod_bslib_server(id)
    }
    
    shinyApp(ui, server)
}
