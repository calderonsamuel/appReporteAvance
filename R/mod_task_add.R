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
    
    get_org_choices <- function() {
        ids <- AppData$orgs |> purrr::map_chr("org_id")
        titles <- AppData$orgs |> purrr::map_chr("org_title")
        setNames(ids, titles)
    }
    
    get_group_choices <- function(org_id) {
        groups <- AppData$groups |> 
            purrr::keep(~.x$org_id == org_id)
        ids <- groups |> purrr::map_chr("group_id")
        titles <- groups |> purrr::map_chr("group_title")
        setNames(ids, titles)
    }
    
    get_user_choices <- function(group_id) {
        users <- AppData$group_users[[group_id]]
        ids <- users |> purrr::map_chr("user_id")
        titles <- users |> purrr::map_chr(~paste(.x$name, .x$last_name))
        setNames(ids, titles)
    }
    
    out_values <- reactiveValues(
        added = 0L
    )
    
    rvalues <- rv()
    rvalues$org_choices <- get_org_choices()
    rvalues$group_choices <- get_group_choices(isolate(rvalues$org_choices[[1]]))
    rvalues$user_choices <- get_user_choices(isolate(rvalues$group_choices[[1]]))
    
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
                choices = rvalues$org_choices),
            selectInput(
                inputId = ns("group_id"), 
                label = "Equipo", 
                choices = rvalues$group_choices),
            selectInput(
                inputId = ns("user_id"),
                label = "Encargado",
                choices = rvalues$user_choices),
            textInput(
                inputId = ns("title"), 
                label = "Título de tarea"
            ),
            textAreaInput(
                inputId = ns("description"), 
                label = "Descripción de tarea"
            ),
            shinyWidgets::airDatepickerInput(
                inputId = ns("time_due"),
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
    
    observe({
        rvalues$group_choices <- get_group_choices(input$org_id)
        updateSelectInput(session, inputId = "group_id",  choices = rvalues$group_choices)
    }) |> bindEvent(input$org_id)
    
    observe({
        rvalues$user_choices <- get_user_choices(input$group_id)
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
