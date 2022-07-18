#' progress UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList radioButtons textAreaInput wellPanel
mod_progress_ui <- function(id){
  ns <- NS(id)
  tagList(
      fluidRow(
          bs4Dash::box(
              title = "Reportar avance de tarea",
              width = 12,
              id = ns("box_reporte"),
              collapsible = FALSE,
              fluidRow(
                  selectInput(
                      inputId = ns("step_id"),
                      label = "Seleccione actividad",
                      choices = c("step_01")
                      # choices = task_in_modal()$step_choices
                  ) |> col_4(),

                  selectInput(
                      inputId = ns("status"),
                      label = "Seleccione nuevo estado",
                      choices = c(
                          "Pendiente",
                          "En proceso",
                          "Pausado",
                          "En revisión",
                          "Terminado"
                      )
                  ) |> col_4(),

                  textInput(
                      inputId = ns("step_explain"),
                      label = "Explique los cambios",
                      placeholder = "Ingrese texto aquí"
                  ) |> col_4()
              ),
              fluidRow(
                  col_2(
                      btn_cancelar(ns("cancelar"), block = TRUE)
                  ),
                  col_2(
                      btn_guardar(ns("modificar"), block = TRUE)
                  )
              )
          ) #|> tagAppendAttributes(style = "display: hidden")
      ),
      verbatimTextOutput(ns("debug")),
    fluidRow(
      bs4Dash::box(
        title = "Pendiente",
        solidHeader = TRUE,
        collapsible = FALSE,
        width = 3,
        icon = icon("calendar"),
        uiOutput(ns("pendientes"))
      ),
      col_3(
        bs4Dash::box(
          title = "En proceso",
          solidHeader = TRUE,
          collapsible = FALSE,
          width = 12,
          icon = icon("pen"),
          # verbatimTextOutput(ns("debug")),
          uiOutput(ns("en_proceso"))
        ),
        bs4Dash::box(
          title = "Pausado",
          solidHeader = TRUE,
          collapsible = FALSE,
          width = 12,
          icon = icon("pause-circle"),
          uiOutput(ns("pausado"))
        ),
      ),
      bs4Dash::box(
        title = "En revisión",
        solidHeader = TRUE,
        collapsible = FALSE,
        width = 3,
        icon = icon("eye"),
          uiOutput(ns("en_revision"))
      ),
      bs4Dash::box(
        title = "Terminado",
        solidHeader = TRUE,
        collapsible = FALSE,
        width = 3,
        icon = icon("check-circle"),
          uiOutput(ns("terminado"))
      )
    )
  )
}

#' progress Server Functions
#'
#' @noRd
mod_progress_server <- function(id, user_iniciado){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # message("Usando mod_progress_server")
    # message(ns("id"))

    # print(output)

    # output$debug <- renderPrint(user_iniciado)

    # bs4Dash::updateBox(id = "box_reporte", action = "remove")

    # message(glue::glue("mod_progress: {user_iniciado}"))
    user_groups <- gruser_get_groups(user_iniciado)
    task_owners <- union(user_iniciado, user_groups)

    rv <- reactiveValues(
        task_list = task_list_from_user(task_owners),
        task_to_modify = NA_character_, # Modificado en observer
        btn_task_id_pressed = 0
    )

    # buttons <- reactiveVal()

    desperate_reactive <- reactive(
        task_ids |>
            lapply(\(x) input[[x]])
    )

    task_ids <- task_get_from_user(task_owners)$task_id

    task_in_modal <- reactive({
        task <- rv$task_list[[rv$task_to_modify]]
        template_id <- task$template$template_id
        template_description <- task$template$template_description
        steps <- step_get_from_template(template_id)

        list(
            status = task$status,
            template_id = template_id,
            template_description = template_description,
            step_id = steps$step_id,
            step_description = steps$step_description,
            step_choices = setNames(steps$step_id, steps$step_description)
        )
    })

    step <- reactiveValues(
        step_choices = reactive(
          setNames(
              object = task_in_modal()$step_id,
              nm = task_in_modal()$step_description
          )
        ),
        status_choices = reactive(
            progress_get_step_status(rv$task_to_modify, input$step_id) |>
            progress_status_choices()
        )
    )

    pendientes <- reactive(task_list_subset_by_status(rv$task_list, "Pendiente"))
    en_proceso <- reactive(task_list_subset_by_status(rv$task_list, "En proceso"))
    pausado <- reactive(task_list_subset_by_status(rv$task_list, "Pausado"))
    en_revision <- reactive(task_list_subset_by_status(rv$task_list, "En revisión"))
    terminado <- reactive(task_list_subset_by_status(rv$task_list, "Terminado"))

    new_progress_data <- reactive(
        data.frame(
            task_id = rv$task_to_modify,
            status_id = ids::proquint(use_openssl = TRUE),
            step_id = input$step_id,
            reported_by = user_iniciado,
            status = input$status,
            time = lubridate::now("America/Lima"),
            explain = input$step_explain
        )
    ) |>
        bindEvent(input$modificar)

    # output$debug <- renderPrint(new_status_data())
    # output$debug <- renderPrint(rv$task_to_modify)

    # Observer de todos los botones "Modificar" en los boxes.
    # modifica rv$task_to_modify y muestra modal dialog
    observe({
        task_ids |>
            lapply(function(x) {
                observe({
                    rv$task_to_modify <- x # set new value

                    step$status_choices <- reactive(
                        progress_get_step_status(rv$task_to_modify, input$step_id) |>
                            progress_status_choices()
                    )

                    message(paste("task to modify is", x))

                    # buttons(buttons() + 1)

                    updateSelectInput(
                        session = session,
                        inputId = "step_id",
                        choices = step$step_choices()
                        # choices = rnorm(3)
                    )

                    updateSelectInput(
                        session = session,
                        inputId = "status",
                        choices = step$status_choices()
                    )

                    updateTextInput(
                        session = session,
                        inputId = "step_explain",
                        value = ""
                    )

                    bs4Dash::updateBox(id = "box_reporte", action = "restore")

                }) |>
                    bindEvent(input[[x]])
            })
    })

    observe({

        if (input$step_explain == "") return(alert_error(session, "Debe explicar cambios"))

        progress_insert(new_progress_data()) |> suppressMessages()

        if (is.na(task_in_modal()$template_id)) {
            task_modify_status(
                task_id = rv$task_to_modify,
                new_status = task_compute_status(rv$task_to_modify)
            ) |> suppressMessages()
        }
        rv$task_list[[rv$task_to_modify]]$status <- input$status
        # rv$task_list[[rv$task_to_modify]]$status |> message()
        # rv$task_list <- task_list_from_user(task_owners)
        alert_info(session, "Estado de actividad actualizado")
        removeModal()

    }) |>
        bindEvent(input$modificar)
        # bindEvent(rv$btn_modify_pressed)

    observe({
        # message("updating status_choices")
        updateSelectInput(
            session = session,
            inputId = "status",
            choices = step$status_choices()
        )
    }) |>
        bindEvent(input$step_id, desperate_reactive())

    observe({
        bs4Dash::updateBox(id = "box_reporte", action = "remove")
    }) |>
        bindEvent(input$cancelar, input$modificar)





    output$pendientes <- renderUI(
        tagList(pendientes() |> lapply(box_group, ns = ns))
    )
    output$en_proceso <- renderUI(
        tagList(en_proceso() |> lapply(box_group, ns = ns))
    )
    output$pausado <- renderUI(
        tagList(pausado() |> lapply(box_group, ns = ns))
    )
    output$en_revision <- renderUI(
        tagList(en_revision() |> lapply(box_group, ns = ns))
    )
    output$terminado <- renderUI(
        tagList(terminado() |> lapply(box_group, ns = ns))
    )

  })
}

mod_progress_testapp <- function(id = "test") {
  ui <- bs4Dash::dashboardPage(
      preloader = list(html = tagList(waiter::spin_pixel(), HTML("<br/>Cargando ...")), color = "#3c8dbc"),
    header = bs4Dash::dashboardHeader(title = "TEST"),
    sidebar = bs4Dash::dashboardSidebar(
      bs4Dash::sidebarMenu(
        bs4Dash::menuItem(text = "Reporte",
                          tabName = "progress",
                          icon = icon("tasks"))
      )
    ),
    body = bs4Dash::dashboardBody(
      bs4Dash::tabItem(tabName = "progress", mod_progress_ui(id))
    )
  )

  server <- function(input, output, session) {
    user_iniciado <- "dgco93@mininter.gob.pe"
    mod_progress_server(id, user_iniciado)
  }

  shinyApp(ui, server)
}

## To be copied in the UI
# mod_progress_ui("progress_1")

## To be copied in the server
# mod_progress_server("progress_1")
