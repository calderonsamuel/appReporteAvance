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
    
    out_values <- reactiveValues(
        added = 0L
    )
    
    rvalues <- rv()
    rvalues$org_choices <- get_org_choices(AppData)
    rvalues$group_choices <- get_group_choices(AppData, isolate(rvalues$org_choices[[1]]))
    rvalues$user_choices <- get_user_choices(AppData, isolate(rvalues$group_choices[[1]]))
    
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
                col_4(selectInput(
                    inputId = ns("output_unit"), 
                    label = "Unidad de medida",
                    choices = c("Documento")
                )),
                column(shinyWidgets::airDatepickerInput(
                    inputId = ns("time_due"),
                    label = "Plazo máximo",
                    value = computeMinTimeDue(tzone = "America/Lima"), 
                    timepicker = TRUE,
                    dateFormat = "dd/mm/yyyy", 
                    language = "es",
                    minDate = computeMinDateDue(tzone = "America/Lima"),
                    maxDate = computeMinDateDue(tzone = "America/Lima") + lubridate::weeks(4),
                    todayButton = TRUE,
                    addon = 'none',
                    timepickerOpts = shinyWidgets::timepickerOptions(
                        minutesStep = 15,
                        minHours = 8,
                        maxHours = 18
                    )
                ), width = 5)
            ),
            
            selectInput(
                inputId = ns("org_id"), 
                label = "Organización", 
                choices = rvalues$org_choices),
            
            fluidRow(
                col_6(selectInput(
                    inputId = ns("group_id"), 
                    label = "Equipo", 
                    choices = rvalues$group_choices)),
                col_6(selectInput(
                    inputId = ns("user_id"),
                    label = "Encargado",
                    choices = rvalues$user_choices))
            ),
            
            footer = tagList(
                modalButton("Cancelar"),
                btn_guardar(ns("save"))
            )
        ))
    }) |> bindEvent(trigger())
    
    observe({
        rvalues$group_choices <- get_group_choices(AppData, input$org_id)
        updateSelectInput(session, inputId = "group_id",  choices = rvalues$group_choices)
    }) |> bindEvent(input$org_id)
    
    observe({
        rvalues$user_choices <- get_user_choices(AppData, input$group_id)
        updateSelectInput(session, inputId = "user_id",  choices = rvalues$user_choices)
    }) |> bindEvent(input$group_id)
    
    observe({
        tryCatch(expr = {
            AppData$task_add(
                org_id = input$org_id,
                group_id = input$group_id,
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
