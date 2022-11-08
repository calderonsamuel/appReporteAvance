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
    
    org_choices <- reactive({
        ids <- AppData$orgs |> purrr::map_chr("org_id")
        titles <- AppData$orgs |> purrr::map_chr("org_title")
        setNames(ids, titles)
    })
    
    group_choices <- reactive({
        ids <- AppData$groups |> purrr::map_chr("group_id")
        titles <- AppData$groups |> purrr::map_chr("group_title")
        setNames(ids, titles)
    })
    
    observe({
        showModal(modalDialog(
            h1("Añadir tarea"),
            selectInput(
                inputId = ns("org_id"), 
                label = "Organización", 
                choices = org_choices()),
            selectInput(
                inputId = ns("group_id"), 
                label = "Equipo", 
                choices = group_choices()),
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
                value = computeMinTimeDue(tzone = "America/Lima"), 
                timepicker = TRUE,
                dateFormat = "dd/mm/yyyy", 
                language = "es",
                minDate = computeMinDateDue(tzone = "America/Lima"),
                maxDate = computeMinDateDue(tzone = "America/Lima") + lubridate::weeks(4),
                todayButton = TRUE,
                timepickerOpts = shinyWidgets::timepickerOptions(
                    minutesStep = 15,
                    minHours = 8,
                    maxHours = 18
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
            ),
            footer = tagList(
                modalButton("Cancelar"),
                btn_guardar(ns("save"))
            )
        ))
    }) |> bindEvent(trigger())
 
  })
}
    
## To be copied in the UI
# mod_task_add_ui("task_add_1")
    
## To be copied in the server
# mod_task_add_server("task_add_1")
