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
        id = ns("sidebar"),
        width = 25,
        h5("Administrar tareas"),
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
      data_tasks = task_get_all(),
      users = user_get_from_privileges(c("user1", "user2"))
    )

    user_choices <- reactive({
        setNames(
            object = vals$users,
            nm = vals$users |> user_get_names()
        )
    })

    templates_choices <- reactive({
        groups <- gruser_get_groups(user_iniciado())
        template_owners <- union(user_iniciado(), groups)
        templates <- template_get_from_user(template_owners)
        setNames(
            object = templates$template_id,
            nm = templates$template_description #|> template_get
        )
    })

    new_task_data <- reactive({
      data.frame(
        reviewer = user_iniciado(),
        user_id = input$user,
        task_id = ids::proquint(use_openssl = TRUE) ,
        # task_id = paste0("T", lubridate::now("America/Lima")),
        task_description = input$description,
        status = "Pendiente"
      )
    })

    task_for_deleting <- reactive(vals$data_tasks$task_id[input$tabla_rows_selected])

    observeEvent(input$add, {
      showModal(modalDialog(
        title = "Nueva tarea",

        selectInput(
          inputId = ns("type"),
          label = "Tipo de asignación",
          choices = c("Usuario" = "user", "Grupo" = "team")
        ),

        selectInput(
          inputId = ns("user"),
          label = "Seleccione encargado",
          choices = user_choices()
          # choices = with(data = user_get_all(),
          #                expr = setNames(object = user_id,
          #                                nm = paste(name, last_name)))
        ),

        shinyWidgets::awesomeCheckbox(
            inputId = ns("use_template"),
            label = "¿Necesita plantilla?",
            status = "info"
        ),

        conditionalPanel(
            condition = "input.use_template",
            ns = ns,
            selectInput(
                inputId = ns("template"),
                label = "Seleccione plantilla",
                choices = templates_choices()
            )
        ),

        textAreaInput(ns("description"), "Descripción de tarea"),

        footer = tagList(
          modalButton("Cancelar"),
          btn_agregar(ns("save"))
        )
      ))
    })

    observeEvent(input$type,{
      if (input$type == "user") {
        updateSelectInput(session, "user", choices = user_choices())
      } else {
        updateSelectInput(session, "user", choices = group_get_all()$group_id)
      }
    })

    observeEvent(input$save, {

      if (input$description == "") {
        alert_error(session, "Debe añadir una descripción")
      } else {
        task_insert(new_task_data())
        vals$data_tasks <- task_get_all()
        updateTextAreaInput(session, "description", value = "")

        removeModal()

        alert_success(session = session, text = "La tarea se añadió correctamente")
      }
    })

    observeEvent(input$remove,{

      if (length(task_for_deleting()) == 0) {
        alert_error(session, "Debe seleccionar una tarea a eliminar")
      } else {
        task_remove(task_for_deleting())
        vals$data_tasks <- task_get_all()
        alert_info(session, "Tarea eliminada")
      }

    })


    output$tabla <- DT::renderDT(
      expr = vals$data_tasks,
      options = options_DT(),
      selection = 'single',
      style = "bootstrap4"
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
