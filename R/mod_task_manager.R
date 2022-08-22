#' task_manager UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_task_man_ui <- function(id) {
    ns <- NS(id)
    btn_agregar(ns("add"), icon = icon("plus"))
}

mod_task_man_output <- function(id, user_iniciado) {
    ns <- NS(id)

    choices_for_tasks <- user_get_choices_for_tasks(user_iniciado)

    user_choices <- choices_for_tasks$user_choices
    template_choices <- choices_for_tasks$template_choices

    tagList(
        bs4Dash::box(
            title = "Agregar nueva tarea",
            # background = "gray",
            width = 12,
            id = ns("box_nueva_tarea"),
            collapsible = FALSE,

            fluidRow(
                col_2(
                    selectInput(
                        inputId = ns("type"),
                        label = "Tipo de asignación",
                        choices = c("Usuario" = "user", "Grupo" = "team")
                    )
                ),
                col_2(
                    selectInput(
                        inputId = ns("user"),
                        label = "Seleccione encargado",
                        choices = user_choices
                    )
                ),
                col_2(
                    selectInput(
                        inputId = ns("use_template"),
                        label = "¿Necesita plantilla?",
                        choices = c("No" = "false",
                                    "Sí" = "true")
                        # status = "info"
                    )
                ),
                col_6(
                    conditionalPanel(
                        condition = "input.use_template === 'true'",
                        ns = ns,
                        selectInput(
                            inputId = ns("template"),
                            label = "Seleccione plantilla",
                            choices = template_choices
                        )
                    )
                )
            ),

            textAreaInput(ns("description"), "Descripción de tarea"),

            fluidRow(
                col_2(
                    btn_cancelar(ns("cancelar"), block = TRUE)
                ),
                col_2(
                    btn_guardar(ns("save"), block = TRUE)
                )
            )
        )
    )
}

#' task_manager Server Functions
#'
#' @noRd
mod_task_man_server <- function(id, user_iniciado) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        bs4Dash::updateBox("box_nueva_tarea", "remove")

        choices_for_tasks <- user_get_choices_for_tasks(user_iniciado)

        task_owners <- user_get_task_owners(user_iniciado)

        vals <- reactiveValues(
            data_tasks = task_get_from_user(task_owners)
        )

        user_choices <- choices_for_tasks$user_choices
        group_choices <- choices_for_tasks$group_choices
        template_choices <- choices_for_tasks$template_choices

        template_id <- reactive({
            ifelse(isTruthy(as.logical(input$use_template)), input$template, NA_character_)
        })

        observeEvent(input$use_template, print(as.logical(input$use_template)))

        step_id <- reactive({
            if (isTruthy(as.logical(input$use_template))) {
                step_get_from_template(template_id())$step_id
            } else {
                "step_01"
            }
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

        observe({
            bs4Dash::updateBox(id = "box_nueva_tarea", action = "restore")
        }) |> bindEvent(input$add)

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

                updateTextAreaInput(session, "description", value = "")

                bs4Dash::updateBox(id = "box_nueva_tarea", action = "remove")

                alert_success(session = session, text = "La tarea se añadió correctamente")
            }
        })

        observe({
            bs4Dash::updateBox(id = "box_nueva_tarea", action = "remove")
        }) |> bindEvent(input$cancelar)

        # return
        reactive(input$save)

    })
}

## To be copied in the UI
# mod_task_manager_ui("task_manager_1")

## To be copied in the server
# mod_task_manager_server("task_manager_1")

mod_task_man_apptest <- function(user_iniciado = "dgco93@mininter.gob.pe") {
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
                bs4Dash::tabItem(
                    tabName = "tasks",
                    mod_task_man_ui("test"),
                    tags$hr(),
                    mod_task_man_output("test", user_iniciado)
                )
            )
        )
    )

    server <- function(input, output, session) {
        mod_task_man_server("test", user_iniciado)
    }

    shinyApp(ui, server)
}
