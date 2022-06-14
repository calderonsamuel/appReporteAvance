#' tasks UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tasks_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::box(
      collapsible = FALSE, width = 12,
      fluidRow(
        tags$span(
          shinyWidgets::dropdownButton(
            icon = icon("plus"), status = "success", inline = TRUE, circle = TRUE,
            tooltip = shinyWidgets::tooltipOptions(title = "Nueva tarea"),

            selectInput(
              ns("user"), "Seleccione encargado",
              choices = with(data = get_users(),
                             expr = setNames(object = user_id,
                                             nm = paste(name, last_name)))
            ),
            textAreaInput(ns("description"), "Descripción de tarea"),
            btn_add(ns("add_task"))
          ),
          btn_trash(ns("delete_task"))
        )
      ),
      fluidRow(DT::DTOutput(ns("tabla")))

    )
  )
}

#' tasks Server Functions
#'
#' @noRd
mod_tasks_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    vals <- reactiveValues(
      data_tasks = get_tasks(),
      users = get_users()$user_id
    )

    new_task_data <- reactive({
      data.frame(
        reviewer = "dgco93",
        user_id = input$user,
        task_id = paste0("T", lubridate::now("America/Lima")),
        task_description = input$description,
        status = "Pendiente"
      )
    })

    output$tabla <- DT::renderDT(
      expr = vals$data_tasks,
      options = options_DT(),
      selection = 'single'
    )

    task_for_deleting <- reactive(vals$data_tasks$task_id[input$tabla_rows_selected])

    output$ids_selected <- renderPrint(task_for_deleting())

    observeEvent(input$add_task, {

      if (input$description == "") {
        alert_error(session, "Debe añadir una descripción")
      } else {
        insert_task(new_task_data())

        vals$data_tasks <- get_tasks()

        # updateSelectInput(session, "user", choices = get_users()$user_id)
        updateTextAreaInput(session, "description", value = "")

        alert_success(session = session, text = "La tarea se añadió correctamente")
      }
    })

    observeEvent(input$delete_task,{

      if (length(task_for_deleting()) == 0) {

        alert_error(session, "Debe seleccionar una tarea a eliminar")

      } else {
        delete_task(task_for_deleting())

        vals$data_tasks <- get_tasks()

        alert_info(session, "Tarea eliminada")
      }

    })

  })
}


mod_tasks_testapp <- function() {
  ui <- fluidPage(
    tags$head(
      shinyWidgets::useSweetAlert()
    ),
    mod_tasks_ui("tasks_1")
  )

  server <- function(input, output, session) {
    mod_tasks_server("tasks_1")
  }

  shinyApp(ui, server)
}

## To be copied in the UI
# mod_tasks_ui("tasks_1")

## To be copied in the server
# mod_tasks_server("tasks_1")
