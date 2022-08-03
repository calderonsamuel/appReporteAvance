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
    btn_add(ns("add")),
    bs4Dash::box(
        title = "Agregar nueva tarea",
        width = 12,
        id = ns("box_nueva_tarea"),
        collapsible = FALSE,



        fluidRow(
            col_2(
                btn_cancelar(ns("cancelar"), block = TRUE)
            ),
            col_2(
                btn_guardar(ns("guardar"), block = TRUE)
            )
        )
    ),

    bs4Dash::box(
      title = "Tareas actuales",
      width = 12,
      sidebar = bs4Dash::boxSidebar(
        id = ns("sidebar"),
        width = 25,
        h5("Administrar tareas"),
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

    privileges <- user_get_privileges(user_iniciado)
    groups <- gruser_get_groups(user_iniciado)
    glue::glue("privileges: {privileges}") |> message()

    users_for_tasks <- if (privileges == "user1") user_iniciado else user_get_from_privileges(c("user1", "user2"))

    task_owners <- union(users_for_tasks, groups)
    glue::glue("task_owners: {vals}", vals = glue::glue_collapse(task_owners, sep = ", ")) |> message()

    template_owners <- union(user_iniciado, groups)
    templates <- template_get_from_user(template_owners)

    vals <- reactiveValues(
      data_tasks = task_get_from_user(task_owners)
    )

    user_choices <- setNames(object = users_for_tasks,
                             nm = users_for_tasks |> purrr::map_chr(user_get_names))

    group_choices <- setNames(object = groups,
                              nm = groups |> purrr::map_chr(group_get_description))

    templates_choices <- setNames(object = templates$template_id,
                                  nm = templates$template_description)

    template_id <- reactive({
        ifelse(isTruthy(input$use_template), input$template, NA_character_)
    })

    step_id <- reactive({
        if (isTruthy(input$use_template)) {
            step_get_from_template(template_id())$step_id
        } else {
            "step_01"
        }
    })

    current_tasks <- reactive({
        data <- vals$data_tasks |>
            subset(select = c(user_id, template_id, task_description, status))
        data$user_id <- purrr::map_chr(data$user_id, user_get_names)

        data |>
            setNames(c("Encargado", "Plantilla", "Descripción de tarea", "Estado actual"))
    })

    new_task_data <- reactive({
      data.frame(
        reviewer = user_iniciado,
        user_id = input$user,
        task_id = ids::proquint(use_openssl = TRUE, n_words = 3) ,
        task_description = input$description,
        template_id = template_id(),
        status = "Pendiente"
      )
    })

    new_progress_data <- reactive({
        data.frame(
            task_id = new_task_data()$task_id,
            status_id = ids::proquint(use_openssl = TRUE),
            step_id = step_id(),
            reported_by = user_iniciado,
            status = "Pendiente",
            time = lubridate::now("America/Lima") |> as.character(),
            explain = "Asignado"
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
          choices = user_choices
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
                choices = templates_choices
            )
        ),

        textAreaInput(ns("description"), "Descripción de tarea"),

        # verbatimTextOutput(ns("debug")),

        footer = tagList(
          modalButton("Cancelar"),
          btn_agregar(ns("save"))
        )
      ))
    })

    # output$debug <- renderPrint(list(template_id(), step_id(), step_get_from_template(input$template)))

    observeEvent(input$type,{
      if (input$type == "user") {
        updateSelectInput(session, "user", choices = user_choices)
      } else {
        updateSelectInput(session, "user", choices = group_choices)
      }
    })

    observeEvent(input$save, {

      if (input$description == "") {
        alert_error(session, "Debe añadir una descripción")
      } else {
        task_insert(new_task_data())
        progress_insert(new_progress_data())
        vals$data_tasks <- task_get_from_user(task_owners)
        updateTextAreaInput(session, "description", value = "")

        removeModal()

        alert_success(session = session, text = "La tarea se añadió correctamente")
      }
    })

    observeEvent(input$remove, {
        if (!isTruthy(task_for_deleting())) {
            alert_error(session, "Debe seleccionar una tarea a eliminar")
        } else if (task_is_from_group(task_for_deleting())){
            alert_error(session, "No puede eliminar tarea de grupo")
        } else {
            task_remove(task_for_deleting())
            vals$data_tasks <- task_get_from_user(task_owners)
            alert_info(session, "Tarea eliminada")
        }

    })

    observe({
        bs4Dash::updateBox(id = "box_nueva_tarea", action = "remove")
    }) |> bindEvent(input$cancelar)


    output$tabla <- DT::renderDT(
      expr = current_tasks(),
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
    user_iniciado <- "dgco93@mininter.gob.pe"
    mod_tasks_server(id, user_iniciado)
  }

  shinyApp(ui, server)
}


## To be copied in the UI
# mod_tasks_ui("tasks_1")

## To be copied in the server
# mod_tasks_server("tasks_1")
