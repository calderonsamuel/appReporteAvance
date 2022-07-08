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
    bs4Dash::box(
      title = "Gestión de usuarios",
      width = 12,
      sidebar = bs4Dash::boxSidebar(
        id = ns("sidebar"),
        width = 25,
        btn_add(ns("add")),
        btn_trash(ns("delete_user"))
      ),
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

    vals <- reactiveValues(
      data_users = user_get_all()
    )

    new_user_data <- reactive({
      data.frame(
        user_id = input$user_id,
        name = input$name,
        last_name = input$last_name,
        privileges = input$privileges,
        responds_to = input$responds_to,
        date_added = as.character(input$date_added)
      )
    })

    user_for_deleting <- reactive({
      vals$data_users$user_id[input$tabla_rows_selected]
    })

    observeEvent(input$add, {
      showModal(modalDialog(
        title = "Nuevo usuario",

        textInput(ns("user_id"), "ID"),
        textInput(ns("name"), "Nombres"),
        textInput(ns("last_name"), "Apellidos"),
        selectInput(ns("privileges"), "Privilegios", choices = c("user1", "user2", "admin")),
        selectInput(ns("responds_to"), "Responde a:", choices = user_get_from_privileges("user2")),
        dateInput(ns("date_added"), "Fecha", language = "es", value = lubridate::today("America/Lima")),

        footer = tagList(
          modalButton("Cancelar"),
          btn_agregar(ns("insert_user"))
        )
      ))
    })

    observeEvent(input$insert_user, {

      user_insert(new_user_data())

      updateTextInput(session, "user_id", value = "")
      updateTextInput(session, "name", value = "")
      updateTextInput(session, "last_name", value = "")
      updateSelectInput(session, "privileges", selected = "user1")
      updateSelectInput(session, "responds_to", choices = user_get_from_privileges("user2"))

      vals$data_users <- user_get_all()

      removeModal()

      alert_info(session = session, sprintf("Se añadió al usuario %s", input$user_id))

    })

    observeEvent(input$delete_user,{
        if (isTruthy(user_for_deleting())) {
            alert_error(session, "Debe seleccionar usuario a eliminar")
        } else {
            user_remove(user_for_deleting())
            alert_info(session, sprintf("Se eliminó al usuario %s", user_for_deleting()))
            vals$data_users <- user_get_all()
        }
    })

    output$tabla <- DT::renderDT(
      expr = vals$data_users,
      options = options_DT(),
      selection = 'single',
      style = "bootstrap4"
    )

  })
}

mod_users_testapp <- function(id = "test") {
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
