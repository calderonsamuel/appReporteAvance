#' reporte UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList radioButtons textAreaInput wellPanel
mod_reporte_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Dash::box(
        title = "Pendiente",
        solidHeader = TRUE,
        collapsible = FALSE,
        width = 3
      ),
      bs4Dash::box(
        title = "En proceso",
        solidHeader = TRUE,
        collapsible = FALSE,
        width = 3
      ),
      col_3(
        bs4Dash::box(
          title = "En revisión",
          solidHeader = TRUE,
          collapsible = FALSE,
          width = 12
        ),
        bs4Dash::box(
          title = "Pausado",
          solidHeader = TRUE,
          collapsible = FALSE,
          width = 12
        ),
      ),
      bs4Dash::box(
        title = "Terminado",
        solidHeader = TRUE,
        collapsible = FALSE,
        width = 3
      )
    )
    # wellPanel(
    #   prettyRadioButtons(
    #     inputId = ns("estado"),
    #     label = "Indique estado de avance",
    #     choices = c("Pendiente", "En proceso", "Postergado/Pausado",
    #                 "Esperando revision", "Terminado"),
    #     selected = character()
    #   ),
    #   textAreaInput(
    #     inputId = ns("descripcion_avance"),
    #     label = "Ingrese descripción del avance",
    #     placeholder = "Sea lo más detallado posible"
    #   )
    # )
  )
}

#' reporte Server Functions
#'
#' @noRd
mod_reporte_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

mod_reporte_testapp <- function() {
  ui <- fluidPage(mod_reporte_ui("reporte_1"))

  server <- function(input, output, session) {
    mod_reporte_server("reporte_1")
  }

  shinyApp(ui, server)
}

## To be copied in the UI
# mod_reporte_ui("reporte_1")

## To be copied in the server
# mod_reporte_server("reporte_1")
