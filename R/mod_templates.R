#' admin_templates UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_templates_ui <- function(id, user_iniciado) {
    ns <- NS(id)
    tagList(
        fluidRow(
            mod_template_manager_ui(ns("template_manager")),
            btn_eliminar(ns("rm_template"), icon = icon("trash")),
            btn_expand(ns("expand"), icon("expand-arrows-alt"))
        ),
        tags$hr(),
        
        mod_template_manager_output(ns("template_manager"), user_iniciado),
        
        bs4Dash::box(
            title = "Plantillas disponibles",
            width = 12,
            height = "600px",
            
            DT::DTOutput(ns("tabla"))
        )
    )
}

#' admin_templates Server Functions
#'
#' @noRd
mod_templates_server <- function(id, rv){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    user_iniciado <- isolate(rv$user_iniciado)
    
    btn_template_added <- mod_template_manager_server("template_manager", user_iniciado)

    group_id <- gruser_get_groups(user_iniciado)
    all_owners <- union(user_iniciado, group_id)

    user_templates <- reactiveVal(template_get_from_user(all_owners) |> template_get_data())

    selected_template <- reactive({
        data <- user_templates()[input$tabla_rows_selected,]
        message("updating selected_template()")
        return(data)
    })

    selected_template_id <- reactive(selected_template()$template_id)

    expanded_template <- reactive(step_get_from_template(selected_template_id()))

    observe({
        if (!isTruthy(selected_template_id())) {
            alert_error(session, "Debe seleccionar una plantilla")
        } else {
            showModal(modalDialog(
                title = "Detalle de plantilla",
                size = "l",
                h4(selected_template()$template_description),
                h6(selected_template()$template_id),
                DT::DTOutput(ns("expanded_template")),
                footer = modalButton("Cerrar")
            ))
        }

    }) |> bindEvent(input$expand)


    observeEvent(input$rm_template, {
      template_remove(selected_template_id())
      step_remove(selected_template_id())

      alert_info(session = session, "Se eliminó la plantilla")

      user_templates(template_get_from_user(all_owners) |> template_get_data())
    })
    
    observe({
        user_templates(template_get_from_user(all_owners) |> template_get_data())
    }) |> bindEvent(btn_template_added())

    

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

mod_templates_apptest <- function(id = "test") {
    user_iniciado <- "dgco93@mininter.gob.pe"
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
      bs4Dash::tabItem(tabName = "templates", mod_templates_ui(id, user_iniciado))
    )
  )

  server <- function(input, output, session) {
      rv <- reactiveValues(
          user_iniciado = user_iniciado
      )
    mod_templates_server(id, rv)
  }

  shinyApp(ui, server)
}

## To be copied in the UI
# mod_templates_ui("admin_templates_1")

## To be copied in the server
# mod_templates_server("admin_templates_1")
