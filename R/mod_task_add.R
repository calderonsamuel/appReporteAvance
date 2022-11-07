#' task_add UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_task_add_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' task_add Server Functions
#'
#' @noRd 
mod_task_add_server <- function(id, AppData, trigger){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observe({
        showModal(modalDialog(
            h1("Añadir tarea"),
            selectInput(
                inputId = ns("org_id"), 
                label = "Organización", 
                choices = head(letters)),
            selectInput(
                inputId = ns("group_id"), 
                label = "Equipo", 
                choices = tail(letters)),
            textInput(
                inputId = ns("title"), 
                label = "Título de tarea"
            ),
            textAreaInput(
                inputId = ns("description"), 
                label = "Descripción de tarea"
            ),
            shinyWidgets::airDatepickerInput(
                inputId = "time_due",
                label = "Plazo máximo",
                value = lubridate::now("America/Lima"), 
                timepicker = TRUE,
                dateFormat = "dd/mm/yyyy", 
                language = "es",
                minDate = lubridate::today("America/Lima"),
                maxDate = lubridate::today("America/Lima") + lubridate::weeks(4),
                todayButton = TRUE,
                timepickerOpts = shinyWidgets::timepickerOptions(
                    minutesStep = 15,
                    minHours = 8,
                    maxHours = 17
                )
            ),
            selectInput(
                inputId = ns("output_unit"), 
                label = "Unidad de medida",
                choices = c("Documento")
            ),
            numericInput(
                inputId = ns("output_goal"),
                label = "Meta",
                value = 1
            )
            
            
        ))
    }) |> bindEvent(trigger())
 
  })
}
    
## To be copied in the UI
# mod_task_add_ui("task_add_1")
    
## To be copied in the server
# mod_task_add_server("task_add_1")
