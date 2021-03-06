#' admin_templates UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_templates_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::box(
      title = "Plantillas disponibles",
      width = 12,
      height = "600px",
      sidebar = bs4Dash::boxSidebar(
        id = ns("sidebar"),
        width = 25,
        # btn_refresh(ns("refresh")),
        btn_add(ns("add_template")),
        btn_trash(ns("rm_template")),
        btn_expand(ns("expand"))
      ),
      div(),
      DT::DTOutput(ns("tabla")),
      div()
    )
  )
}

#' admin_templates Server Functions
#'
#' @noRd
mod_templates_server <- function(id, user_iniciado){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    group_id <- gruser_get_groups(user_iniciado)
    all_owners <- union(user_iniciado, group_id)

    step_count <- reactiveVal(1L)
    step_list_numbers <- reactive(seq_len(step_count()))
    user_templates <- reactiveVal(template_get_from_user(all_owners))

    selected_template <- reactive({
        data <- user_templates()[input$tabla_rows_selected,]
        message("updating selected_template()")
        return(data)
    })

    selected_template_id <- reactive(selected_template()$template_id)

    expanded_template <- reactive(step_get_from_template(selected_template_id()))

    template_id <- reactive({
      if (input$template_id == "") {
          ids::proquint(n_words = 3, use_openssl = TRUE)
      } else {
        input$template_id
      }
    })

    new_template_data <- reactive({
      data.frame(
        template_id = template_id(),
        template_description = input$temp_description,
        user_id = input$template_owner
      )
    })

    new_template_steps_data <- reactive({
      step_list_numbers() |>
        lapply(function(x) {
          data.frame(
            user_id = input$template_owner,
            template_id = template_id(),
            step_id = sprintf("step_%02i", x),
            step_description = input[[sprintf("step_%02i", x)]]
          )
        }) |>
        (\(x) do.call(what = rbind, args = x))() # ugly but owrks in R 4.1
        # do.call(what = rbind, args = _) # needs R 4.2
    })

    observeEvent(input$add_template, {

        showModal(modalDialog(
          title = "Añadir tareas a plantilla",
          textInput(inputId = ns("temp_description"),
                    label = "Nombre de plantilla",
                    placeholder = "Ingresar nombre de plantilla"),
          selectInput(
            inputId = ns("template_owner"),
            label = "Seleccione dueño de plantilla",
            # choices = letters[1:5]
            choices = setNames(all_owners, all_owners |> purrr::map_chr(user_get_names))
          ),
          checkboxInput(ns("plantilla_mapro"), label = "¿Es proceso MAPRO?"),
          conditionalPanel(
              condition = "input.plantilla_mapro",
              ns = ns,
              textInput(inputId = ns("template_id"),
                        label = "ID de proceso",
                        placeholder = "Solo llenar en procesos MAPRO")
          ),
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

    observe({

        showModal(modalDialog(
            title = "Detalle de plantilla",
            size = "l",
            h4(selected_template()$template_description),
            h6(selected_template()$template_id),
            DT::DTOutput(ns("expanded_template")),
            footer = modalButton("Cerrar")
        ))

    }) |> bindEvent(input$expand)

    observeEvent(input$add_step, step_count(step_count() + 1L))

    observeEvent(input$rm_step, if (step_count() > 1L) step_count(step_count() - 1L))

    observeEvent(input$save_template_steps, {

      step_insert(new_template_steps_data())
      template_insert(new_template_data())

      removeModal()

      updateTextInput(session = session,
                      inputId = "temp_description",
                      value = "")

      updateTextInput(session = session,
                      inputId = "template_id",
                      value = "")

      # Resetear todos los inputs de step
      step_list_numbers() |>
        lapply(function(x) {
          updateTextInput(session = session,
                          inputId = sprintf("step_%02i", x),
                          value = "")
        })

      alert_success(session, "Plantilla añadida")

      user_templates(template_get_from_user(all_owners))


    })

    observeEvent(input$rm_template, {
      template_remove(selected_template_id())
      step_remove(selected_template_id())

      alert_info(session = session, "Se eliminó la plantilla")

      user_templates(template_get_from_user(all_owners))
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

    output$tabla <- DT::renderDT(
      expr = user_templates(),
      options = options_DT(),
      selection = 'single',
      style = "bootstrap4"
    )

    output$expanded_template <- DT::renderDT(
        expr = expanded_template() |> subset(select = c(step_id, step_description)),
        options = options_DT(),
        selection = 'single',
        style = "bootstrap4"
    )


  })
}

mod_templates_testapp <- function(id = "test") {
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
      bs4Dash::tabItem(tabName = "templates", mod_templates_ui(id))
    )
  )

  server <- function(input, output, session) {
    user_iniciado <- "dgco93@mininter.gob.pe"
    mod_templates_server(id, user_iniciado)
  }

  shinyApp(ui, server)
}

## To be copied in the UI
# mod_templates_ui("admin_templates_1")

## To be copied in the server
# mod_templates_server("admin_templates_1")
