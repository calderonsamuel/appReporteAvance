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
        inputId = ns("add"),
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
mod_processes_server <- function(id, app_data, group_selection){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    rv <- reactiveValues(
      processes_need_refresh = 0L
    )

    processes <- reactive({
      app_data$fetch_processes()
    }) |>
      bindEvent(rv$processes_need_refresh, group_selection$group_selected())

    output$processes <- renderUI({
      lapply(processes(), \(x) {
        process_display(
          item = x,
          editInputId = ns("processToEdit"),
          deleteInputId = ns("processToDelete")
        )
      })
    })

    # Add ----

    observe({
      showModal(modalDialog(
        title = "Nuevo proceso",
        size = "l",

        textInputPro(
          inputId = ns("title"),
          label = "Nombre de proceso",
          maxlength = 250,
          maxlengthCounter = TRUE
        ),
        textAreaInputPro(
          inputId = ns("description"),
          label = "Descripción",
          maxlength = 500,
          maxlengthCounter = TRUE
        ),

        footer = tagList(
          modalButton("Cancelar"),
          btn_guardar(ns("save"))
        )
      ))
    }) |>
      bindEvent(input$add)

    observe({
      tryCatch({
        app_data$process_add(
          title = input$title,
          description = input$description
        )

        showNotification("Proceso añadido", duration = 3, session = session)
        removeModal(session)
        rv$processes_need_refresh <- rv$processes_need_refresh + 1L

      }, error = \(e) alert_error(session, e))
    }) |>
    bindEvent(input$save)

    # Delete ----

    observe({
      tryCatch({
        shinyWidgets::ask_confirmation(
          inputId = ns("confirm_delete"),
          title = "Eliminar proceso",
          text = "El equipo ya no tendrá acceso al proceso y sus unidades de medida",
          type = "warning",
          btn_labels = c("Cancelar", "Confirmar"),
          btn_colors = c("#6e7d88", "#ff5964")
        )
      }, error = \(e) alert_error(session, e))
    }) |>
      bindEvent(input$processToDelete)

    observe({
      tryCatch({
        if(isTRUE(input$confirm_delete)) {
          app_data$process_delete(
              process_id = input$processToDelete
          )
          
          rv$processes_need_refresh <- rv$processes_need_refresh + 1L
          
          alert_info(session, "Proceso eliminado")
        }
          
      }, error = \(e) alert_error(session, e))
    }) |> 
      bindEvent(input$confirm_delete)

    # Edit ----

    observe({

      process_to_edit <- processes() |>
        purrr::keep(~.x$process_id == input$processToEdit)  |>
        purrr::pluck(1)

      showModal(modalDialog(
        title = "Editar proceso",
        size = "l",

        textInputPro(
          inputId = ns("title"),
          label = "Nombre de proceso",
          value = process_to_edit$title,
          maxlength = 250,
          maxlengthCounter = TRUE
        ),
        textAreaInputPro(
          inputId = ns("description"),
          label = "Descripción",
          value = process_to_edit$description,
          maxlength = 500,
          maxlengthCounter = TRUE
        ),

        footer = tagList(
          modalButton("Cancelar"),
          btn_guardar(ns("save_edition"))
        )
      ))
    }) |>
      bindEvent(input$processToEdit)

    observe({
      tryCatch({
        app_data$process_edit(
          process_id = input$processToEdit,
          title = input$title,
          description = input$description
        )

        showNotification("Proceso editado", duration = 3, session = session)
        removeModal(session)
        rv$processes_need_refresh <- rv$processes_need_refresh + 1L

      }, error = \(e) alert_error(session, e))
    }) |>
    bindEvent(input$save_edition)
    
    # output ----
    list(processes = processes)

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
