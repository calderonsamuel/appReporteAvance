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
      title = "Mis plantillas",
      width = 12,
      collapsed = TRUE
    ),
    bs4Dash::box(
      title = "Nueva plantilla",
      width = 12,
      fluidRow(
        col_10(textInput(inputId = ns("temp_description"),
                         label = NULL,
                         placeholder = "Nombre de plantilla")),
        col_1(btn_add(ns("add_template"))),
        col_1(btn_trash(ns("rm_template")))
      )#,
      # tableOutput(ns("tabla"))
    )
  )
}

#' admin_templates Server Functions
#'
#' @noRd
mod_admin_templates_server <- function(id, user_iniciado){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # R/reactive_utils.R
    # step_count <- reactive_counter(input$add_step, input$rm_step,
    #                                start_value = 1L, min_value = 1L)

    step_count <- reactiveVal(1L)
    step_list_numbers <- reactive(seq_len(step_count()))
    template_id <- reactiveVal(character())

    observeEvent(input$add_template, {
      if (input$temp_description == "") {
        alert_error(session = session, text = "¡Debe poner un nombre de plantilla!")
      } else {
        # Set new value for template id
        template_id(paste0("TEMPLATE_", lubridate::now("America/Lima")))

        showModal(modalDialog(
          # title = "Añadir tareas a plantilla",
          h2("Plantilla nueva:"),
          h4(input$temp_description),
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
      }


    })

    observeEvent(input$add_step, step_count(step_count() + 1L))

    observeEvent(input$rm_step, if (step_count() > 1L) step_count(step_count() - 1L))

    observeEvent(input$save_template_steps, {

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

      removeModal()
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

    output$step_list <- renderUI({
      step_list_numbers() |>
        lapply(function(x) {
          textInput(
            inputId = ns(sprintf("step_%02i", x)),
            label = NULL,
            placeholder = sprintf("Describa tarea %02i", x),
            value = isolate(input[[sprintf("step_%02i", x)]])
          )
        })
    })

    output$tabla <- renderTable({
      validate(need(input[["step_01"]], "Sin datos de plantilla nueva"))
      template_steps_data()
    })


  })
}

mod_admin_templates_testapp <- function(id = "test") {
  ui <- bs4Dash::dashboardPage(
    header = bs4Dash::dashboardHeader(title = "TEST"),
    sidebar = bs4Dash::dashboardSidebar(
      bs4Dash::sidebarMenu(
        bs4Dash::menuItem(text = "Admin Templates",
                          tabName = "templates")
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
