#' board UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_board_ui <- function(id) {
    ns <- NS(id)
    
    tagList(
        fluidRow(
            bs4Dash::box(
                title = "Pendiente",
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 3,
                headerBorder = FALSE,
                class = "cw cw-lg",
                icon = icon("calendar"),
                dropdownMenu = bs4Dash::boxDropdown(
                    icon = fontawesome::fa("fas fa-ellipsis"),
                    bs4Dash::boxDropdownItem(
                        "Agregar",
                        icon = fontawesome::fa("fas fa-circle-plus"),
                        id = ns("task_add")
                    )
                ),
                uiOutput(ns("pendientes"))
            ),
            col_3(
                bs4Dash::box(
                    title = "En proceso",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    headerBorder = FALSE,
                    width = 12,
                    class = "cw cw-sm",
                    icon = icon("pen"),
                    uiOutput(ns("en_proceso"))
                ),
                bs4Dash::box(
                    title = "Pausado",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    headerBorder = FALSE,
                    width = 12,
                    class = "cw cw-sm",
                    icon = icon("pause-circle"),
                    uiOutput(ns("pausado"))
                ),
            ),
            bs4Dash::box(
                title = "En revisión",
                solidHeader = TRUE,
                collapsible = FALSE,
                headerBorder = FALSE,
                width = 3,
                class = "cw cw-lg",
                icon = icon("eye"),
                uiOutput(ns("en_revision"))
            ),
            bs4Dash::box(
                title = "Terminado",
                solidHeader = TRUE,
                collapsible = FALSE,
                headerBorder = FALSE,
                width = 3,
                class = "cw cw-lg",
                icon = icon("check-circle"),
                uiOutput(ns("terminado"))
            )
        ),
        
        
        mod_task_add_ui(ns("task_add_1")),
        verbatimTextOutput(ns("debug"))
    )
}

#' board Server Functions
#'
#' @noRd
mod_board_server <- function(id, AppData) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        # Modules ----
        
        task_gets_added <- mod_task_add_server(
            id = "task_add_1", 
            AppData = AppData, 
            trigger = reactive(input$task_add)
        )
        
        # Reactives ----
        
        tasks <- reactive(AppData$tasks) |> 
            bindEvent(
                task_gets_added(),
                rv$task_has_been_deleted,
                rv$task_has_been_reported,
                rv$task_has_been_edited
            )
        
        rv <- reactiveValues(
            task_to_delete = list(),
            task_has_been_deleted = 0L,
            task_to_report = list(),
            task_has_been_reported = 0L,
            task_to_edit = list(),
            task_has_been_edited = 0L
        )
        
        
        delete_btns <- reactive({
            ids <- tasks() |> 
                purrr::map_chr("task_id") |> 
                purrr::map_chr(~paste0(.x, "-task-delete"))
            
            ids 
        })
        
        report_btns <- reactive({
            ids <- tasks() |> 
                purrr::map_chr("task_id") |> 
                purrr::map_chr(~paste0(.x, "-task-report"))
            
            ids 
        })
        
        edit_btns <- reactive({
            ids <- tasks() |> 
                purrr::map_chr("task_id") |> 
                purrr::map_chr(~paste0(.x, "-task-edit"))
            
            ids 
        })
        
        # Observers ----
        
        ## Deleting task ----
        
        observe({
            delete_btns() |> 
                purrr::walk(~
                    observe({
                        task_id <- stringr::str_remove(.x, "-task-delete")
                        rv$task_to_delete <- tasks()[[task_id]]
                        
                        shinyWidgets::ask_confirmation(
                            inputId = ns("confirm_delete"),
                            title = "Eliminar tarea", 
                            text = "Se eliminará también cualquier progreso asociado. No se podrá recuperar la información.", 
                            type = "warning", 
                            btn_labels = c("Cancelar", "Confirmar"),
                            btn_colors = c("#6e7d88", "#ff5964")
                        )
                    }) |> bindEvent(input[[.x]])
                )
        })
        
        observe({
            tryCatch(expr = {
                if(isTRUE(input$confirm_delete)) {
                    AppData$task_delete(
                        process_id = rv$task_to_delete$process_id, 
                        activity_id = rv$task_to_delete$activity_id, 
                        org_id = rv$task_to_delete$org_id, 
                        group_id = rv$task_to_delete$group_id, 
                        task_id = rv$task_to_delete$task_id)
                    
                    rv$task_has_been_deleted <- rv$task_has_been_deleted + 1L
                }
            }, error = \(e) alert_error(session, e))
            
        }) |> 
            bindEvent(input$confirm_delete)
        
        ## Reporting progress ----
        
        observe({
            report_btns() |> 
                purrr::walk(~
                    observe({
                        task_id <- stringr::str_remove(.x, "-task-report")
                        rv$task_to_report <- tasks()[[task_id]]
                        
                        showModal(modalDialog(
                            h1("Reporte de avance"),
                            selectInput(
                                inputId = ns("report_status_current"),
                                label =  "Nuevo estado", 
                                choices = task_get_status_choices(rv$task_to_report$status_current)),
                            numericInput(
                                inputId = ns("report_output_current"), 
                                label = paste0("Avance actual (", rv$task_to_report$output_unit, ")"),
                                value = rv$task_to_report$output_current,
                                min = rv$task_to_report$output_current,
                                max = rv$task_to_report$output_goal
                            ),
                            textAreaInput(
                                inputId = ns("report_details"), 
                                label = "Detalles"
                            ),
                            
                            footer = tagList(
                                modalButton("Cancelar"),
                                btn_guardar(ns("save_report"))
                            )
                        ))
                    }) |> bindEvent(input[[.x]], ignoreInit = TRUE)
                )
        })
        
        observe({
            tryCatch(expr = {
                AppData$task_report_progress(
                    process_id = rv$task_to_report$process_id, 
                    activity_id = rv$task_to_report$activity_id, 
                    org_id = rv$task_to_report$org_id, 
                    group_id = rv$task_to_report$group_id, 
                    task_id = rv$task_to_report$task_id,
                    status_current = input$report_status_current,
                    output_current = input$report_output_current,
                    details = input$report_details)
                
                
                removeModal(session)
                
                rv$task_has_been_reported <- rv$task_has_been_reported + 1L
                
            }, error = \(e) alert_error(session, e))
                
        }) |> 
            bindEvent(input$save_report)
        
        ## Editing task metadata
        
        observe({
            edit_btns() |> 
                purrr::walk(~
                    observe({
                        task_id <- stringr::str_remove(.x, "-task-edit")
                        rv$task_to_edit <- tasks()[[task_id]]
                        
                        showModal(modalDialog(
                            
                            h1("Editar tarea"),
                            
                            textInput(
                                inputId = ns("edit_title"),
                                label = "Título de tarea",
                                value = rv$task_to_edit$task_title
                            ),
                            textAreaInput(
                                inputId = ns("edit_description"),
                                label = "Descripción de tarea",
                                value = rv$task_to_edit$task_description
                            ),
                            
                            footer = tagList(
                                modalButton("Cancelar"),
                                btn_guardar(ns("save_edition"))
                            )
                        ))
                    }) |> bindEvent(input[[.x]], ignoreInit = TRUE)
                )
        })
        
        observe({
            tryCatch(expr = {
                AppData$task_edit_metadata(
                    process_id = rv$task_to_edit$process_id, 
                    activity_id = rv$task_to_edit$activity_id, 
                    org_id = rv$task_to_edit$org_id, 
                    group_id = rv$task_to_edit$group_id, 
                    task_id = rv$task_to_edit$task_id,
                    task_title = input$edit_title,
                    task_description = input$edit_description)
                
                
                removeModal(session)
                
                rv$task_has_been_edited <- rv$task_has_been_edited + 1L
                
            }, error = \(e) alert_error(session, e))
            
        }) |> 
            bindEvent(input$save_edition)
        
        
        # Outputs ----
        
        output$pendientes <- renderUI({
            tasks() |> 
                purrr::keep(~ .x$status_current == "Pendiente") |> 
                lapply(task_box, ns) |>
                tagList()
        }) 
        
        output$en_proceso <- renderUI({
            tasks() |> 
                purrr::keep(~ .x$status_current == "En proceso") |> 
                lapply(task_box, ns) |>
                tagList()
        })
        
        output$pausado <- renderUI({
            tasks() |> 
                purrr::keep(~ .x$status_current == "Pausado") |> 
                lapply(task_box, ns) |>
                tagList()
        })
        
        output$en_revision <- renderUI({
            tasks() |> 
                purrr::keep(~ .x$status_current == "En revisión") |> 
                lapply(task_box, ns) |>
                tagList()
        })
        
        output$terminado <- renderUI({
            tasks() |> 
                purrr::keep(~ .x$status_current == "Terminado") |> 
                lapply(task_box, ns) |>
                tagList()
        })
        
        # Debug ----
        
        # output$debug <- renderPrint({
        #     rv$task_to_delete
        # })
        
    })
}
    
## To be copied in the UI
# mod_board_ui("board_1")
    
## To be copied in the server
# mod_board_server("board_1")

mod_board_apptest <- function() {
    AppData <- AppData$new("dgco93@mininter.gob.pe")
    id = ids::random_id()
    quick_bs4dash(
        modUI = mod_board_ui(id = id),
        modServer = mod_board_server(id = id, AppData)
    )
}


task_get_status_choices <- function(status) {
    switch (status,
        "Pendiente" = c("En proceso", "En revisión"),
        "En proceso" = c("En proceso", "Pausado", "En revisión"),
        "Pausado" = c("En proceso", "En revisión"),
        "En revisión" = c("En proceso", "Terminado"),
        "Terminado" = c("Archivado")
    )
}
