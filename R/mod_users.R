#' admin_users UI Inputs Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_users_ui <- function(id) {
  ns <- NS(id)
  tagList(
      mod_user_manager_btns(ns("user_manager")),
      btn_eliminar(ns("delete_user"), icon = icon("trash")),
      tags$hr(),
      mod_user_manager_inputs(ns("user_manager")),
    bs4Dash::box(
      title = "Usuarios registrados",
      width = 12,
      DT::DTOutput(ns("tabla"))
    )
  )
}


#' admin_users Server Functions
#'
#' @noRd
mod_users_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    btn_usuario_agregado <- mod_user_manager_server("user_manager")

    vals <- reactiveValues(
      data_users = user_get_all()
    )

    selected_user <- reactive({
      vals$data_users$user_id[input$tabla_rows_selected]
    })

    observeEvent(input$delete_user,{
        if (!isTruthy(selected_user())) {
            alert_error(session, "Debe seleccionar usuario a eliminar")
        } else {
            user_remove(selected_user())
            alert_info(session, sprintf("Se eliminó al usuario %s", selected_user()))
            vals$data_users <- user_get_all()
        }
    })

    observe({
        vals$data_users <- user_get_all()
    }) |> bindEvent(btn_usuario_agregado())

    output$tabla <- DT::renderDT(
      expr = vals$data_users,
      options = options_DT(),
      selection = 'single',
      style = "bootstrap4"
    )

  })
}

mod_users_apptest <- function(id = "test") {
  ui <- bs4Dash::dashboardPage(
    header = bs4Dash::dashboardHeader(title = "TEST"),
    sidebar = bs4Dash::dashboardSidebar(
      bs4Dash::sidebarMenu(
        bs4Dash::menuItem(text = "Admin USers",
                          tabName = "users",
                          icon = icon("user-edit"))
      )
    ),
    body = bs4Dash::dashboardBody(
      bs4Dash::tabItem(tabName = "users", mod_users_ui(id))
    )
  )

  server <- function(input, output, session) {
    mod_users_server(id)
  }

  shinyApp(ui, server)
}

## To be copied in the UI
# mod_users_input("admin_users_1")
# mod_users_output("admin_users_1")

## To be copied in the server
# mod_users_server("admin_users_1")
