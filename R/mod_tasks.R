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
    sidebarLayout(
      sidebarPanel(
        h3("Nueva tarea"),
        selectInput(ns("user"), "Seleccione encargado", choices = get_users()$user_id),
        textInput(ns("description"), "Descripción de tarea"),
        btn_add(ns("add_task")),
        h3("Eliminar tarea"),
        btn_trash(ns("delete_task"))
      ),
      mainPanel(
        # verbatimTextOutput(ns("ids_selected")),
        DT::DTOutput(ns("tabla"))
      )
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
        user = input$user,
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

      insert_task(new_task_data())

      vals$data_tasks <- get_tasks()

      updateSelectInput(session, "user", choices = get_users()$user_id)
      updateTextInput(session, "description", value = "")
      updateSelectInput(session, "select_task", choices = vals$data_tasks$task_id)

      showModal(modalDialog(
        title = "Nueva tarea añadida",
        footer = modalButton("Ok")
      ))
    })

    observeEvent(input$delete_task,{

      if (length(task_for_deleting()) == 0) {

        showModal(modalDialog(
          title = "Debe seleccionar una tarea a eliminar",
          footer = modalButton("Ok")
        ))

      } else {
        delete_task(task_for_deleting())

        vals$data_tasks <- get_tasks()

        # updateSelectInput(session, "select_task", choices = vals$data_tasks$task_id)

        showModal(modalDialog(
          title = "Tarea eliminada",
          footer = modalButton("Ok")
        ))
      }

    })

  })
}


mod_tasks_testapp <- function() {
  ui <- fluidPage(
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
