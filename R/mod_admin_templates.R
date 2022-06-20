#' admin_templates UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_admin_templates_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::box(
      title = "Plantillas disponibles",
      width = 12,
      height = "600px",
      sidebar = bs4Dash::boxSidebar(
        id = "sidebar",
        width = 25,
        btn_refresh(ns("refresh")),
        btn_add(ns("add_template")),
        btn_trash(ns("rm_template"))
      ),
      div(),
      DT::DTOutput(ns("table")),
      div()
    )
  )
}

#' admin_templates Server Functions
#'
#' @noRd
mod_admin_templates_server <- function(id, user_iniciado){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    step_count <- reactiveVal(1L)
    step_list_numbers <- reactive(seq_len(step_count()))
    user_templates <- reactiveVal(data.frame())
    template_id <- reactive({
      if (input$template_id == "") {
        paste0("TEMPLATE_", lubridate::now("America/Lima"))
      } else {
        input$template_id
      }
    })

    template_data <- reactive({
      data.frame(
        template_id = template_id(),
        template_description = input$temp_description,
        user_id = user_iniciado()
      )
    })

    template_steps_data <- reactive({
      step_list_numbers() |>
        lapply(function(x) {
          data.frame(
            user_id = user_iniciado(),
            template_id = template_id(),
            step_id = sprintf("step_%02i", x),
            step_description = input[[sprintf("step_%02i", x)]]
          )
        }) |>
        do.call(what = rbind, args = _)
    })

    observeEvent(input$refresh, {
      user_templates(get_templates_from_user(user_iniciado()))
    })

    observeEvent(input$add_template, {

        showModal(modalDialog(
          title = "Añadir tareas a plantilla",
          h5("Nombre de plantilla"),
          textInput(inputId = ns("temp_description"),
                    label = NULL,
                    placeholder = "Ingresar nombre de plantilla"),
          h5("ID de plantilla (opcional)"),
          textInput(inputId = ns("template_id"),
                    label = NULL,
                    placeholder = "Opcional"),
          # h2("Plantilla nueva:"),
          h5("Ingresar tareas"),
          fluidRow(
            col_10(
              uiOutput(ns("step_list")),
            ),
            col_1(btn_add(ns("add_step"), size = "sm")),
            col_1(btn_minus(ns("rm_step"), size = "sm"))
          ),
          footer = tagList(
            modalButton("Cancelar"),
            btn_agregar(ns("save_template_steps"))
          )
        ))

    })

    observeEvent(input$add_step, step_count(step_count() + 1L))

    observeEvent(input$rm_step, if (step_count() > 1L) step_count(step_count() - 1L))

    observeEvent(input$save_template_steps, {

      insert_template_steps(template_steps_data())
      insert_template(template_data())

      removeModal()

      updateTextInput(session = session,
                      inputId = "temp_description",
                      value = "")

      # Resetear todos los inputs de step
      step_list_numbers() |>
        lapply(function(x) {
          updateTextInput(session = session,
                          inputId = sprintf("step_%02i", x),
                          value = "")
        })

      user_templates(get_templates_from_user(user_iniciado()))

    })

    observeEvent(input$rm_template, {
      delete_template(template_id())
      delete_template_steps(template_id())

      alert_info(session = session, "Se eliminó la plantilla")

      user_templates(get_templates_from_user(user_iniciado()))
    })

    output$step_list <- renderUI({
      step_list_numbers() |>
        lapply(function(x) {
          textInput(
            inputId = ns(sprintf("step_%02i", x)),
            label = NULL,
            placeholder = sprintf("Ingresar tarea %02i", x),
            value = isolate(input[[sprintf("step_%02i", x)]])
          )
        })
    })

    output$table <- DT::renderDT(
      expr = user_templates(),
      options = options_DT(),
      selection = 'single'
    )


  })
}

mod_admin_templates_testapp <- function(id = "test") {
  ui <- bs4Dash::dashboardPage(
    header = bs4Dash::dashboardHeader(title = "TEST"),
    sidebar = bs4Dash::dashboardSidebar(
      bs4Dash::sidebarMenu(
        bs4Dash::menuItem(text = "Admin Templates",
                          tabName = "templates",
                          icon = icon("book"))
      )
    ),
    body = bs4Dash::dashboardBody(
      bs4Dash::tabItem(tabName = "templates", mod_admin_templates_ui(id))
    )
  )

  server <- function(input, output, session) {
    mod_admin_templates_server(id, user_iniciado = reactive("dgco93@mininter.gob.pe"))
  }

  shinyApp(ui, server)
}

## To be copied in the UI
# mod_admin_templates_ui("admin_templates_1")

## To be copied in the server
# mod_admin_templates_server("admin_templates_1")
