#' admin_users UI Inputs Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_admin_users_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      col_4(
        bs4Dash::box(
          width = 12,
          bs4Dash::box(
            title = "Agregar usuario",
            width = 12,
            collapsed = TRUE,
            status = "success",
            textInput(ns("user_id"), "ID"),
            textInput(ns("name"), "Nombres"),
            textInput(ns("last_name"), "Apellidos"),
            selectInput(ns("privileges"), "Privilegios", choices = c("user1", "user2", "admin")),
            selectInput(ns("responds_to"), "Responde a:", choices = get_user_id_from_privileges("user2")),
            dateInput(ns("date_added"), "Fecha", language = "es", value = lubridate::today("America/Lima")),
            btn_agregar(ns("insert_user"))
          ),
          bs4Dash::box(
            title = "Eliminar usuario",
            width = 12,
            status = "danger",
            collapsed = TRUE,
            uiOutput(ns("select_user")),
            btn_eliminar(ns("delete_user"))
          )
        )
      ),
      col_8(
        bs4Dash::box(
          width = 12,
          h3("Tabla de usuarios"),
          DT::DTOutput(ns("table_users"))
        )
      )
    )
  )
}


#' admin_users Server Functions
#'
#' @noRd
mod_admin_users_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    vals <- reactiveValues(data_users = get_users())

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

    observeEvent(input$insert_user, {

      insert_user(new_user_data())

      updateTextInput(session, "user_id", value = "")
      updateTextInput(session, "name", value = "")
      updateTextInput(session, "last_name", value = "")
      updateSelectInput(session, "privileges", selected = "user1")
      updateSelectInput(session, "responds_to", choices = get_user_id_from_privileges("user2"))

      vals$data_users <- get_users()

      alert_info(session = session, sprintf("Se añadió al usuario %s", input$user_id))

    })

    observeEvent(input$delete_user,{

      delete_user(input$del_user_id)

      vals$data_users <- get_users()

      alert_info(session, sprintf("Se eliminó al usuario %s", input$user_id))

    })

    output$table_users <- DT::renderDT(
      expr = vals$data_users,
      options = options_DT(),
      selection = 'single'
    )

    output$select_user <- renderUI({
      selectInput(inputId = ns("del_user_id"),
                  label = "Elegir usuario a eliminar",
                  choices = rev(vals$data_users$user_id))
    })

  })
}

## To be copied in the UI
# mod_admin_users_input("admin_users_1")
# mod_admin_users_output("admin_users_1")

## To be copied in the server
# mod_admin_users_server("admin_users_1")
