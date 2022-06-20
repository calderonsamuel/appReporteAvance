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
      title = "Tareas actuales",
      width = 12,
      sidebar = bs4Dash::boxSidebar(
        id = "sidebar",
        width = 25,
        btn_add(ns("add")),
        btn_trash(ns("remove"))
      ),
      DT::DTOutput(ns("tabla"))
    )
  )
}

#' tasks Server Functions
#'
#' @noRd
mod_tasks_server <- function(id, user_iniciado){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    vals <- reactiveValues(
      data_tasks = get_tasks(),
      users = get_users()$user_id
    )

    new_task_data <- reactive({
      data.frame(
        reviewer = user_iniciado(),
        user_id = input$user,
        task_id = paste0("T", lubridate::now("America/Lima")),
        task_description = input$description,
        status = "Pendiente"
      )
    })

    task_for_deleting <- reactive(vals$data_tasks$task_id[input$tabla_rows_selected])

    observeEvent(input$add, {
      showModal(modalDialog(
        title = "Nueva tarea",

        selectInput(
          inputId = ns("user"),
          label = "Seleccione encargado",
          choices = with(data = get_users(),
                         expr = setNames(object = user_id,
                                         nm = paste(name, last_name)))
        ),
        textAreaInput(ns("description"), "Descripción de tarea"),

        footer = tagList(
          modalButton("Cancelar"),
          btn_agregar(ns("save"))
        )
      ))
    })

    observeEvent(input$save, {

      if (input$description == "") {
        alert_error(session, "Debe añadir una descripción")
      } else {
        insert_task(new_task_data())
        vals$data_tasks <- get_tasks()
        # updateSelectInput(session, "user", choices = get_users()$user_id)
        updateTextAreaInput(session, "description", value = "")

        removeModal()

        alert_success(session = session, text = "La tarea se añadió correctamente")
      }
    })

    observeEvent(input$remove,{

      if (length(task_for_deleting()) == 0) {
        alert_error(session, "Debe seleccionar una tarea a eliminar")
      } else {
        delete_task(task_for_deleting())
        vals$data_tasks <- get_tasks()
        alert_info(session, "Tarea eliminada")
      }

    })


    output$tabla <- DT::renderDT(
      expr = vals$data_tasks,
      options = options_DT(),
      selection = 'single'
    )

  })
}

mod_tasks_testapp <- function(id = "test") {
  ui <- tagList(
    tags$head(
      shinyWidgets::useSweetAlert()
    ),
    bs4Dash::dashboardPage(
      header = bs4Dash::dashboardHeader(title = "TEST"),
      sidebar = bs4Dash::dashboardSidebar(
        bs4Dash::sidebarMenu(
          bs4Dash::menuItem(text = "Tareas",
                            tabName = "tasks",
                            icon = icon("calendar-plus"))
        )
      ),
      body = bs4Dash::dashboardBody(
        bs4Dash::tabItem(tabName = "tasks", mod_tasks_ui(id))
      )
    )
  )

  server <- function(input, output, session) {
    user_iniciado <- reactive("dgco93@mininter.gob.pe")
    mod_tasks_server(id, user_iniciado)
  }

  shinyApp(ui, server)
}


## To be copied in the UI
# mod_tasks_ui("tasks_1")

## To be copied in the server
# mod_tasks_server("tasks_1")
