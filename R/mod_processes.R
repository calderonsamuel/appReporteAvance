#' processes UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_processes_ui <- function(id){
  ns <- NS(id)
  tagList(
    h5("Gestión de procesos"),
      btn_custom(
        inputId = ns("add_process"),
        label = "Agregar",
        icon = fontawesome::fa("fas fa-plus"),
        class = "btn-success btn-sm mb-2"
      ),
      uiOutput(ns("processes"))
  )
}
    
#' processes Server Functions
#'
#' @noRd 
mod_processes_server <- function(id, app_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    rv <- reactiveValues(
      processes_need_refresh = 0L
    )

    processes <- reactive({
      app_data$fetch_processes()
    }) |>
      bindEvent(rv$processes_need_refresh)

    output$processes <- renderUI({
      lapply(processes(), \(x) {
        process_display(
          item = x,
          editInputId = ns("processToEdit"),
          deleteInputId = ns("processToDelete")
        )
      })
    })



  })
}
    
## To be copied in the UI
# mod_processes_ui("processes_1")
    
## To be copied in the server
# mod_processes_server("processes_1")

process_display <- function(item, editInputId, deleteInputId) {
    div(
        class = "row p-1 mx-0 mb-2 mw-100",
        style = "background-color: #FFFFFF33; border-radius: 5px;",
        div(
            class = "col d-flex align-items-center pl-2",
            `data-toggle`= "tooltip",
            `data-placement`= "top",
            title = paste0("Descripción: ", item$description),
            item$title
        ),
        div(
            class = "col-xs-auto d-flex align-items-center",
            admin_toolbar(
                editInputId = editInputId,
                deleteInputId = deleteInputId,
                value = item$process_id
            )
        )
    )
}
