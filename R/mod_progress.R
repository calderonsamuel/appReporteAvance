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

    groups <- reactive({
        user_iniciado() |>
            gruser_get_groups()
    })

    all_owners <- reactive(union(user_iniciado(), groups()))

    last_btn_pressed <- reactiveVal(character()) # Modificado en observer

    task_list <- reactiveValues(
        all = reactive(task_list_from_user(all_owners()))
    )

    # Se asume que los id de tarea no cambian, aunque cambien sus estados
    task_ids <- reactive(task_get_from_user(all_owners())$task_id)

    task_in_modal <- reactive({
        task <- task_list$all()[[last_btn_pressed()]]
        template_id <- task$template$template_id
        template_description <- task$template$template_description
        steps <- step_get_from_template(template_id)

        list(
            status = task$status,
            # assignee = task$assignee$user_id,
            template_id = template_id,
            template_description = template_description,
            step_choices = setNames(steps$step_id, steps$step_description)
        )
    })

    step <- reactiveValues(
        status_choices = reactive(
            progress_get_step_status(last_btn_pressed(), input$step_id) |>
            progress_status_choices()
        )
    )

    pendientes <- reactive(task_list_subset_by_status(task_list$all(), "Pendiente"))
    en_proceso <- reactive(task_list_subset_by_status(task_list$all(), "En proceso"))
    pausado <- reactive(task_list_subset_by_status(task_list$all(), "Pausado"))
    en_revision <- reactive(task_list_subset_by_status(task_list$all(), "En revisión"))
    terminado <- reactive(task_list_subset_by_status(task_list$all(), "Terminado"))

    new_progress_data <- reactive(
        data.frame(
            task_id = last_btn_pressed(),
            status_id = ids::proquint(use_openssl = TRUE),
            step_id = input$step_id,
            reported_by = user_iniciado(),
            status = input$status,
            time = lubridate::now("America/Lima"),
            explain = input$step_explain
        )
    ) |>
        bindEvent(input$modificar)

    # output$debug <- renderPrint(new_status_data())
    # output$debug <- renderPrint(last_btn_pressed())

    # Observer de todos los botones "Modificar" en los boxes.
    # modifica last_btn_pressed y muestra modal dialog
    observe({
        task_ids() |>
            lapply(function(x) {
                observe({
                    last_btn_pressed(x)
                    showModal(modalDialog(
                        title = "Reportar avance de tarea",

                        selectInput(
                            inputId = ns("step_id"),
                            label = "Seleccione actividad",
                            choices = task_in_modal()$step_choices
                        ),

                        selectInput(
                            inputId = ns("status"),
                            label = "Seleccione nuevo estado",
                            choices = ""
                        ),

                        textAreaInput(
                            inputId = ns("step_explain"),
                            label = "Explique los cambios"
                        ),

                        # verbatimTextOutput(ns("debug")),

                        footer = tagList(
                                        modalButton("Cancelar"),
                                        btn_modificar(ns("modificar"))
                                    )

                    ))
                }) |>
                    bindEvent(input[[x]])
            })
    })


    observe({

        progress_insert(new_progress_data())

        if (is.na(task_in_modal()$template_id)) {
            task_modify_status(
                task_id = last_btn_pressed(),
                new_status = input$status
            )
        }

        removeModal()

        task_list$all <- reactive(union(user_iniciado(), groups()) |>
                                      task_list_from_user())
        alert_info(session, "Estado de actividad actualizado")

    }) |>
        bindEvent(input$modificar)

    observe({
        updateSelectInput(
            session = session,
            inputId = "status",
            choices = step$status_choices()
        )
    }) |>
        bindEvent(input$step_id)

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
    user_iniciado <- reactive("dgco93@mininter.gob.pe")
    mod_progress_server(id, user_iniciado)
  }

  shinyApp(ui, server)
}

## To be copied in the UI
# mod_progress_ui("progress_1")

## To be copied in the server
# mod_progress_server("progress_1")
