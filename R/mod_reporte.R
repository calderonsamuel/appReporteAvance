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
    wellPanel(
      prettyRadioButtons(
        inputId = ns("estado"),
        label = "Indique estado de avance",
        choices = c("Pendiente", "En proceso", "Postergado/Pausado",
                    "Esperando revision", "Terminado"),
        selected = character()
      ),
      textAreaInput(
        inputId = ns("descripcion_avance"),
        label = "Ingrese descripción del avance",
        placeholder = "Sea lo más detallado posible"
      )
    )
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

## To be copied in the UI
# mod_reporte_ui("reporte_1")

## To be copied in the server
# mod_reporte_server("reporte_1")
