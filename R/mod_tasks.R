#' tasks UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tasks_ui <- function(id, user_iniciado) {
    ns <- NS(id)

    tagList(
        mod_task_man_ui(ns("task_manager")),
        btn_eliminar(ns("remove"), icon = icon("trash")),

        tags$hr(),

        mod_task_man_output(ns("task_manager"), user_iniciado),

        bs4Dash::box(
            title = "Tareas actuales",
            width = 12,
            DT::DTOutput(ns("tabla"))
        )


    )
}

#' tasks Server Functions
#'
#' @noRd
mod_tasks_server <- function(id, rv){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    user_iniciado <- isolate(rv$user_iniciado)

    task_added <- mod_task_man_server("task_manager", user_iniciado)

    choices_for_tasks <- user_get_choices_for_tasks(user_iniciado)

    task_owners <- user_get_task_owners(user_iniciado)

    vals <- reactiveValues(
      data_tasks = task_get_from_user(task_owners)
    )

    user_choices <- choices_for_tasks$user_choices
    group_choices <- choices_for_tasks$group_choices
    template_choices <- choices_for_tasks$template_choices

    current_tasks <- reactive({
        data <- vals$data_tasks |>
            subset(select = c(user_id, template_id, task_description, status))
        data$user_id <- purrr::map_chr(data$user_id, user_get_names)

        data |>
            setNames(c("Encargado", "Plantilla", "Descripción de tarea", "Estado actual"))
    })

    task_for_deleting <- reactive(vals$data_tasks$task_id[input$tabla_rows_selected])

    observeEvent(input$remove, {
        if (!isTruthy(task_for_deleting())) {
            alert_error(session, "Debe seleccionar una tarea a eliminar")
        } else if (task_is_from_group(task_for_deleting())){
            alert_error(session, "No puede eliminar tarea de grupo")
        } else {
            task_remove(task_for_deleting())
            vals$data_tasks <- task_get_from_user2(task_owners) |> task_get_from_id()
            alert_info(session, "Tarea eliminada")
        }

    })

    observe({
        vals$data_tasks <- task_get_from_user2(task_owners) |> task_get_from_id()
    }) |> bindEvent(task_added())

    output$tabla <- DT::renderDT(
      expr = current_tasks(),
      options = options_DT(),
      selection = 'single',
      style = "bootstrap4"
    )

  })
}

mod_tasks_apptest <- function(user_iniciado = "dgco93@mininter.gob.pe") {
  ui <- tagList(
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
        golem_add_external_resources(),
        bs4Dash::tabItem(tabName = "tasks", mod_tasks_ui("test", user_iniciado))
      )
    )
  )

  server <- function(input, output, session) {
      rv <- reactiveValues(
          user_iniciado = user_iniciado
      )
    mod_tasks_server("test", rv)
  }

  shinyApp(ui, server)
}


## To be copied in the UI
# mod_tasks_ui("tasks_1")

## To be copied in the server
# mod_tasks_server("tasks_1")


