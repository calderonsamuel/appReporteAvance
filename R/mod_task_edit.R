#' task_edit UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_task_edit_ui <- function(id){
  ns <- NS(id)
  tagList(
    
  )
}
    
#' task_edit Server Functions
#'
#' @noRd 
mod_task_edit_server <- function(id, app_data, rv){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Edit metadata ----

    observe({
      showModal(modalDialog(
          
        h1("Editar tarea"),
        
        textInputPro(
          inputId = ns("edit_title"),
          label = "Título de tarea",
          value = rv$task_to_edit$task_title,
          maxlength = 250,
          maxlengthCounter = TRUE
        ),
        textAreaInputPro(
          inputId = ns("edit_description"),
          label = "Descripción de tarea",
          value = rv$task_to_edit$task_description,
          maxlength = 500,
          maxlengthCounter = TRUE
        ),

        timeDuePicker(
          inputId = ns("time_due"),
          label = "Plazo máximo",
          value = lubridate::with_tz(rv$task_to_edit$time_due, "America/Lima")
        ),
        
        footer = tagList(
          modalButton("Cancelar"),
          btn_guardar(ns("save_edition"))
        )
      ))
    }) |> bindEvent(rv$task_to_edit)



    observe({
        tryCatch(expr = {
            app_data$task_edit_metadata(
              task_id = rv$task_to_edit$task_id,
              task_title = input$edit_title,
              task_description = input$edit_description
            )

            if (input$time_due != rv$task_to_edit$time_due) {
              app_data$task_edit_time_due(
                task_id = rv$task_to_edit$task_id,
                time_due = lubridate::with_tz(input$time_due, "UTC")
              )
            }
            
            removeModal(session)
            
            rv$task_has_been_edited <- rv$task_has_been_edited + 1L
            
        }, error = \(e) {
          alert_error(session, e)
          cli::cli_inform(e)
        })
        
    }) |> 
        bindEvent(input$save_edition)



  })
}
    
## To be copied in the UI
# mod_task_edit_ui("task_edit_1")
    
## To be copied in the server
# mod_task_edit_server("task_edit_1")
