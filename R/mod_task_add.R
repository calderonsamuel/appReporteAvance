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
mod_task_add_server <- function(id, app_data, controlbar){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    out_values <- reactiveValues(
        added = 0L
    )
    
    user_choices <- reactive({
        get_user_choices(app_data, controlbar$group_selected())
    })

    processes_choices <- reactive({
        all_processes = controlbar$processes()
        values <- purrr::map_chr(all_processes, "process_id")
        names <- purrr::map_chr(all_processes, "title")

        setNames(values, names)
    })

    unit_choices <- reactive({
        app_data$fetch_units(input$process) |> 
            purrr::keep(~.x$type == "task") |> 
            purrr::map_chr("unit_title") |>
            unname()
    })
    
    observe({
        showModal(modalDialog(
            h1("Añadir tarea"),
            size = "l",
            textInputPro(
                inputId = ns("title"), 
                label = "Título de tarea",
                width = "100%",
                maxlength = 250,
                maxlengthCounter = TRUE
            ),
            textAreaInputPro(
                inputId = ns("description"), 
                label = "Descripción de tarea",
                width = "100%",
                maxlength = 500,
                maxlengthCounter = TRUE
            ),
            
            fluidRow(
                col_6(selectInput(
                    inputId = ns("process"),
                    label = "Proceso",
                    choices = processes_choices(),
                    width = "100%"
                )),
                col_6(shinyWidgets::pickerInput(
                    inputId = ns("output_unit"), 
                    label = "Unidad de medida",
                    width = "100%",
                    choices = unit_choices(),
                    options = shinyWidgets::pickerOptions(
                        liveSearch = TRUE
                    )
                ))
            ),
            fluidRow(
                col_6(timeDuePicker(
                    inputId = ns("time_due"),
                    label = "Plazo máximo",
                    width = "100%",
                    value = computeMinTimeDue(tzone = "America/Lima") + lubridate::weeks(1)
                )),
                col_6(selectInput(
                    inputId = ns("user_id"),
                    label = "Encargado",
                    width = "100%",
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
        cli::cli_alert_info("Task add time due: {input$time_due}")
    }) |>
        bindEvent(input$time_due)
    
    observe({
        tryCatch(expr = {
            app_data$task_add(
                group_id = controlbar$group_selected(),
                task_title = input$title,
                task_description = input$description,
                assignee = input$user_id,
                time_due = lubridate::with_tz(input$time_due, "UTC"),
                output_unit = input$output_unit
            )
            
            removeModal(session)
            
            alert_success(session, "Tarea agregada")
            
            out_values$added <- out_values$added + 1L
            
        }, error = \(e) alert_error(session, e))
    }) |> bindEvent(input$save)

    # update unit choices ----
    observe({
        shinyWidgets::updatePickerInput(
            session = session,
            inputId = "output_unit",
            choices = unit_choices()
        )
    }) |>
        bindEvent(input$process)
    
    # output
    
    reactive(out_values$added)
 
  })
}
    
## To be copied in the UI
# mod_task_add_ui("task_add_1")
    
## To be copied in the server
# mod_task_add_server("task_add_1")

# convertir esto en una funcion wrapper, debe tomar id y valor
timeDuePicker <- function(inputId, label, value, width = NULL) {
    shinyWidgets::airDatepickerInput(
        inputId = inputId,
        label = label,
        value = value, 
        width = width,
        timepicker = TRUE,
        dateFormat = "dd/MM/yyyy", 
        language = "es",
        minDate = computeMinDateDue(tzone = "America/Lima"),
        maxDate = computeMinDateDue(tzone = "America/Lima") + lubridate::period(6, "months"),
        todayButton = TRUE, 
        firstDay = 0,
        addon = 'none',
        timepickerOpts = shinyWidgets::timepickerOptions(
            minutesStep = 15,
            minHours = 8,
            maxHours = 18
        )
    )
}