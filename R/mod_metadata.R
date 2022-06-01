#' metadata UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList radioButtons textInput conditionalPanel wellPanel
#' @importFrom shinyWidgets prettyRadioButtons
mod_metadata_ui <- function(id){
  actividades_operativas <- c(
    "EJECUCIÓN DE LA CUSTODIA, TRASLADO Y ENTREGA DE LA DROGA DECOMISADA",
    "RECEPCION, ALMACENAMIENTO Y DESTRUCCIÓN DE LA DROGA DECOMISADA",
    "DESTRUCCION Y DISPOSICION FINAL DE INSUMOS QUIMICOS Y PRODUCTOS FISCALIZADOS",
    "REDUCCION DE HECTAREAS DE PLANTACIONES ILEGALES DE COCA Y DESTRUCCION DE ALMACIGOS DE COCA",
    "EJECUCIÓN DE LA ASISTENCIA A LA POBLACION POR EL PROGRAMA DE RESPONSABILIDAD SOCIAL",
    "DIAGNÓSTICO DE LA CADENA DE RESULTADOS Y FORMULACIÓN DE LA ESTRATEGIA SECTORIAL DE LUCHA CONTRA LAS DROGAS"
  )

  ns <- NS(id)

  tagList(
    wellPanel(
      prettyRadioButtons(
        inputId = ns("act_operativa"),
        label = "Seleccione actividad operativa",
        # choices = c(paste("Act", seq_len(6)), 'Otro')
        choiceNames = c(actividades_operativas, 'Otro'),
        choiceValues = seq_len(7),
        selected = 1
      ),
      conditionalPanel(
        condition = "input.act_operativa == 7",
        ns = ns,
        textInput(
          inputId = ns("act_operativa_otro"),
          label = NULL,
          placeholder = "Indique actividad no POI"
        )
      ),
      prettyRadioButtons(
        inputId = ns("act_especifica"),
        label = "Seleccione actividad específica",
        # choices = NULL
        # choiceNames = paste0("Act 1.", seq_len(4)),
        # choiceValues = 1:4
        choices = paste0("Act 1.", seq_len(4))
      )
    )
  )
}

#' metadata Server Functions
#'
#' @importFrom shiny observeEvent
#' @importFrom shinyWidgets updatePrettyRadioButtons
#'
#' @noRd
mod_metadata_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$act_operativa,{
      updatePrettyRadioButtons(
        session = session,
        inputId = "act_especifica",
        label = "Seleccione actividad específica",
        choices = paste0("Act ", input$act_operativa, ".", seq_len(4))
      )

    })

  })
}

## To be copied in the UI
# mod_metadata_ui("metadata_1")

## To be copied in the server
# mod_metadata_server("metadata_1")
