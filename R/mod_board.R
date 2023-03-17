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
                    mod_task_add_ui(ns("task_add_1")), # this is a dropdown-item
                    mod_report_add_ui(ns("report_add_1"))
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
            col_3(
                bs4Dash::box(
                    title = "En revisión",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    headerBorder = FALSE,
                    width = 12,
                    class = "cw cw-sm",
                    icon = icon("eye"),
                    uiOutput(ns("en_revision"))
                ),
                bs4Dash::box(
                    title = "Observado",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    headerBorder = FALSE,
                    width = 12,
                    class = "cw cw-sm",
                    icon = fontawesome::fa("far fa-hand"),
                    uiOutput(ns("observado"))
                )
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
        
        
        verbatimTextOutput(ns("debug"))
    )
}

#' board Server Functions
#'
#' @noRd
mod_board_server <- function(id, app_data, controlbar) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        # Modules ----
        
        task_gets_added <- mod_task_add_server("task_add_1", app_data, controlbar)
        report_gets_added <- mod_report_add_server("report_add_1", app_data, controlbar)
        
        # Reactives ----
        
        tasks <- reactive({
            app_data$tasks 
        }) |> 
            bindEvent(
                task_gets_added(),
                rv$task_has_been_deleted,
                rv$task_has_been_reported,
                rv$task_has_been_edited,
                controlbar$group_selected(),
                controlbar$group_colors_modified()
            )
        
        reports <- reactive({
            app_data$reports
        }) |> 
            bindEvent(
                report_gets_added()
            )
        
        rv <- reactiveValues(
            task_to_delete = list(),
            task_has_been_deleted = 0L,
            task_to_report = list(),
            task_has_been_reported = 0L,
            task_to_edit = list(),
            task_has_been_edited = 0L,
            task_to_history = list()
        )
        
        # Observers ----
        
        ## Deleting task ----
        
        observe({
            rv$task_to_delete <- tasks()[[input$taskToDelete]]
            
            shinyWidgets::ask_confirmation(
                inputId = ns("confirm_delete"),
                title = "Eliminar tarea", 
                text = "Se eliminará también cualquier progreso asociado. No se podrá recuperar la información.", 
                type = "warning", 
                btn_labels = c("Cancelar", "Confirmar"),
                btn_colors = c("#6e7d88", "#ff5964")
            )
        }) |> bindEvent(input$taskToDelete)
        
        observe({
            tryCatch(expr = {
                if(isTRUE(input$confirm_delete)) {
                    app_data$task_delete(task_id = rv$task_to_delete$task_id)
                    
                    rv$task_has_been_deleted <- rv$task_has_been_deleted + 1L
                    
                    showNotification(session = session, "Tarea eliminada", duration = 3, type = "message")
                }
            }, error = \(e) alert_error(session, e))
            
        }) |> 
            bindEvent(input$confirm_delete)
        
        ## Reporting progress ----
        
        observe({
            rv$task_to_report <- tasks()[[input$taskToReport]]
            
            showModal(modalDialog(
                h1("Reporte de avance"),
                tags$p(paste0("Tarea: ", rv$task_to_report$task_title)),
                tags$p(paste0("Descripción: ", rv$task_to_report$task_description)),
                
                selectInput(
                    inputId = ns("report_status_current"),
                    label =  "Nuevo estado", 
                    choices = task_get_status_choices(rv$task_to_report$status_current),
                    width = "100%"
                ),
                numericInput(
                    inputId = ns("report_output_current"), 
                    label = paste0("Avance actual - ", rv$task_to_report$output_unit),
                    value = rv$task_to_report$output_current,
                    min = rv$task_to_report$output_current,
                    max = rv$task_to_report$output_goal,
                    width = "100%"
                ),
                textAreaInput(
                    inputId = ns("report_details"), 
                    label = "Detalles",
                    width = "100%"
                ),
                
                footer = tagList(
                    modalButton("Cancelar"),
                    btn_guardar(ns("save_report"))
                )
            ))
        }) |> bindEvent(input$taskToReport)
        
        observe({
            tryCatch(expr = {
                app_data$task_report_progress(
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
            rv$task_to_edit <- tasks()[[input$taskToEdit]]
            
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
        }) |> bindEvent(input$taskToEdit)
        
        observe({
            tryCatch(expr = {
                app_data$task_edit_metadata(
                    task_id = rv$task_to_edit$task_id,
                    task_title = input$edit_title,
                    task_description = input$edit_description)
                
                
                removeModal(session)
                
                rv$task_has_been_edited <- rv$task_has_been_edited + 1L
                
            }, error = \(e) alert_error(session, e))
            
        }) |> 
            bindEvent(input$save_edition)
        
        ## See history ----
        
        observe({
            rv$task_to_history <- tasks()[[input$taskToHistory]]
            
            showModal(modalDialog(
                
                h1("Historial de progreso"),
                
                tags$p("Tarea:", rv$task_to_history$task_title),
                tags$p("Unidad de medida:", rv$task_to_history$output_unit),
                
                reactable:::reactableOutput(ns("table_history")),
                
                footer = tagList(
                    modalButton("Cerrar")
                ),
                size = "l"
            ))
        }) |> bindEvent(input$taskToHistory)
        
        # Outputs ----
        
        output$pendientes <- renderUI({
            task_box_by_status(tasks(), "Pendiente", ns, controlbar$is_admin())
        }) 
        
        output$en_proceso <- renderUI({
            task_box_by_status(tasks(), "En proceso", ns, controlbar$is_admin())
        })
        
        output$pausado <- renderUI({
            task_box_by_status(tasks(), "Pausado", ns, controlbar$is_admin())
        })
        
        output$en_revision <- renderUI({
            task_box_by_status(tasks(), "En revisión", ns, controlbar$is_admin())
        })
        
        output$observado <- renderUI({
            task_box_by_status(tasks(), "Observado", ns, controlbar$is_admin())
        })
        
        output$terminado <- renderUI({
            tagList(
                task_box_by_status(tasks(), "Terminado", ns, controlbar$is_admin()),
                reports() |> purrr::map(report_box)
            )
        })
        
        
        output$table_history <- reactable::renderReactable({
            app_data$task_get_history(rv$task_to_history$task_id) |> 
                purrr::pmap(list) |> 
                purrr::map_dfr(~data.frame(
                    "Fecha" = format(.x$time_reported, "%d/%m/%Y %H:%M:%S"),
                    "Por" = .x$user_names,
                    "Estado" = .x$status,
                    "Progreso" = .x$output_progress,
                    "Detalle" = .x$details
                )) |> 
                reactable::reactable()
        })
        
        # Debug ----
        # 
        # output$debug <- renderPrint({
        #     config |>
        #         purrr::map(~.x())
        # })
        
    })
}
    
## To be copied in the UI
# mod_board_ui("board_1")
    
## To be copied in the server
# mod_board_server("board_1")

mod_board_apptest <- function(email = Sys.getenv("REPORTES_EMAIL")) {
    app_data <- AppData$new(email)
    id = ids::random_id()
    quick_bs4dash(
        modUI = mod_board_ui(id = id),
        modServer = mod_board_server(id = id, app_data, controlbar = fake_config(app_data))
    )
}


task_get_status_choices <- function(status) {
    switch (status,
        "Pendiente" = c("En proceso", "En revisión"),
        "En proceso" = c("En proceso", "Pausado", "En revisión"),
        "Pausado" = c("En proceso", "En revisión"),
        "En revisión" = c("En proceso", "Observado", "Terminado"),
        "Observado" = c("En revisión"),
        "Terminado" = c("Archivado")
    )
}

task_box_by_status <- function(tasks, status, ns, is_group_admin) {
    tasks |> 
        purrr::keep(~ .x$status_current == status) |> 
        purrr::map(~ task_box(
            task = .x,
            ns = ns, 
            is_group_admin = is_group_admin
        )) |>
        tagList()
}
