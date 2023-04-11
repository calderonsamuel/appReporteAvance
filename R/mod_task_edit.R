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
          maxlengthCounter = TRUE,
          width = "100%"
        ),
        textAreaInputPro(
          inputId = ns("edit_description"),
          label = "Descripción de tarea",
          value = rv$task_to_edit$task_description,
          maxlength = 500,
          maxlengthCounter = TRUE,
          width = "100%"
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
            # timeDuePicker() receives `value` in tz: America/Lima. 
            # So its input value should be treated like it has the same timezone
            time_due_in_UTC <- input$time_due |> 
                lubridate::force_tz("America/Lima") |> 
                lubridate::with_tz("UTC")

            title_modified <- input$edit_title != rv$task_to_edit$task_title
            description_modified <- input$edit_description != rv$task_to_edit$task_description
            time_due_modified <- time_due_in_UTC != lubridate::with_tz(rv$task_to_edit$time_due, "UTC")

            if (!any(title_modified, description_modified, time_due_modified)) {
              return(showNotification("Sin modificación en tarea", session = session))
            }

            app_data$task_edit(
              task_id = rv$task_to_edit$task_id,
              task_title = if (title_modified) input$edit_title else NULL,
              task_description = if (description_modified) input$edit_description else NULL,
              time_due = if (time_due_modified) time_due_in_UTC else NULL
            )
            
            removeModal(session)
            
            rv$task_has_been_edited <- rv$task_has_been_edited + 1L

            showNotification("Tarea modificada", session = session)
            
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
