#' task_report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_task_report_ui <- function(id) {
    ns <- NS(id)
    tagList(
        
    )
}

#' task_report Server Functions
#'
#' @noRd
mod_task_report_server <- function(id, TaskData, trigger) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        
    })
}
    
## To be copied in the UI
# mod_task_report_ui("task_report_1")
    
## To be copied in the server
# mod_task_report_server("task_report_1")
