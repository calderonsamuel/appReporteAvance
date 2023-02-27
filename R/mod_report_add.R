#' report_add UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_report_add_ui <- function(id) {
    ns <- NS(id)
    tagList(
        htmltools::a(
            class = "dropdown-item action-button",
            href = "#",
            id = ns("add"),
            fontawesome::fa("fas fa-file-circle-check"),
            "Nuevo reporte"
        )
    )
}

#' report_add Server Functions
#'
#' @noRd
mod_report_add_server <- function(id, AppData, controlbar) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        module_output <- reactiveValues(
            added = 0L
        )
        
        unit_choices <- reactive({
            units <- AppData$group_units |> purrr::keep(~ .x$type == "report")
            ids <- units |> purrr::map_chr("unit_id")
            titles <- units |> purrr::map_chr("unit_title")
            
            setNames(ids, titles)
        }) |> 
            bindEvent(input$add)
        
        observe({
            showModal(modalDialog(
                title = "Nuevo reporte",
                
                footer = tagList(
                    modalButton("Cancelar"),
                    btn_guardar(ns("save"))
                )
            ))
        }) |> 
            bindEvent(input$add)
        
        observe({
            tryCatch({
                
                removeModal(session)
                module_output$added <- module_output$added + 1L
                alert_success(session, "Reporte agregado")
                
            }, error = \(e) alert_error(session, e))
        }) |> 
            bindEvent(input$save)
        
        # output ----
        
        reactive(module_output$added)
    })
}

## To be copied in the UI
# mod_report_add_ui("report_add_1")

## To be copied in the server
# mod_report_add_server("report_add_1")
