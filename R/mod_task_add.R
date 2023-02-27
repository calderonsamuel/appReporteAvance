#' task_add UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_task_add_ui <- function(id) {
    ns <- NS(id)
    tagList(
        htmltools::a(
            class = "dropdown-item action-button",
            href = "#",
            id = ns("add"),
            fontawesome::fa("fas fa-list-check"),
            "Nueva tarea"
        )
    )
}
    
#' task_add Server Functions
#'
#' @noRd 
mod_task_add_server <- function(id, AppData, controlbar){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    out_values <- reactiveValues(
        added = 0L
    )
    
    user_choices <- reactive({
        get_user_choices(AppData, controlbar$group_selected())
    })
    
    observe({
        showModal(modalDialog(
            h1("Añadir tarea"),
            textInput(
                inputId = ns("title"), 
                label = "Título de tarea",
                width = "100%"
            ),
            textAreaInput(
                inputId = ns("description"), 
                label = "Descripción de tarea",
                width = "100%"
            ),
            
            fluidRow(
                col_3(numericInput(
                    inputId = ns("output_goal"),
                    label = "Meta",
                    value = 1
                )),
                column(shinyWidgets::pickerInput(
                    inputId = ns("output_unit"), 
                    label = "Unidad de medida",
                    width = "100%",
                    choices = output_unit_choices(),
                    options = shinyWidgets::pickerOptions(
                        liveSearch = TRUE
                    )
                ), width = 9)
            ),
            fluidRow(
                col_6(shinyWidgets::airDatepickerInput(
                    inputId = ns("time_due"),
                    label = "Plazo máximo",
                    value = computeMinTimeDue(tzone = "America/Lima") + lubridate::weeks(1), 
                    timepicker = TRUE,
                    dateFormat = "dd/MM/yyyy", 
                    language = "es",
                    minDate = computeMinDateDue(tzone = "America/Lima"),
                    maxDate = computeMinDateDue(tzone = "America/Lima") + lubridate::weeks(4),
                    todayButton = TRUE,
                    firstDay = 0,
                    addon = 'none',
                    timepickerOpts = shinyWidgets::timepickerOptions(
                        minutesStep = 15,
                        minHours = 8,
                        maxHours = 18
                    )
                )),
                col_6(selectInput(
                    inputId = ns("user_id"),
                    label = "Encargado",
                    choices = user_choices()
                ))
            ),
            
            footer = tagList(
                modalButton("Cancelar"),
                btn_guardar(ns("save"))
            )
        ))
    }) |> bindEvent(input$add)
    
    observe({
        tryCatch(expr = {
            AppData$task_add(
                org_id = controlbar$org_selected(),
                group_id = controlbar$group_selected(),
                task_title = input$title,
                task_description = input$description,
                assignee = input$user_id,
                time_due = lubridate::with_tz(input$time_due, "America/Lima"),
                output_unit = input$output_unit,
                output_goal = input$output_goal
            )
            
            removeModal(session)
            
            alert_success(session, "Tarea agregada")
            
            out_values$added <- out_values$added + 1L
            
        }, error = \(e) alert_error(session, e))
    }) |> bindEvent(input$save)
    
    # output
    
    reactive(out_values$added)
 
  })
}
    
## To be copied in the UI
# mod_task_add_ui("task_add_1")
    
## To be copied in the server
# mod_task_add_server("task_add_1")

output_unit_choices <- function() {
    list(ADMINISTRATIVO = c("Informe", "Proyecto de informe", "Proyecto de Memorando", 
                            "Proyecto de Oficio", "Ayuda memoria", "PPT", "Entregable", "Correo"
    ), DESTRUCCIÓN = c("Informe", "Proyecto de hoja de recomendación", 
                       "Proyecto de plan de operaciones", "Proyecto de oficio para destrucción (VOI)", 
                       "Anexo por tipo de droga", "Relación de droga para destrucción", 
                       "kg de droga destruida, por tipo de droga", "kg de droga destruida, por tipo de droga", 
                       "Proyecto de Informe de destrucción para VOI", "Informe final de destrucción"
    ), INTERNAMIENTO = c("Pericias revisadas", "Pericias corregidas", 
                         "Consolidado de Relación de drogas", "Relación de drogas revisadas", 
                         "Relación de drogas corregidas", "Hoja de trabajo para internamiento", 
                         "Proyecto de informe de internamiento", "Kg de droga internada, por tipo de droga", 
                         "Muestras programadas para recepción",
                         "Muestras observadas en recepción",
                         "Muestras observadas en almacenamiento",
                         "Bolsas almacenadas"))
}
