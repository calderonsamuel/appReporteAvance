#' admin_users UI Inputs Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_admin_users_input <- function(id){
  ns <- NS(id)
  tagList(
    h3("Agregar nuevo usuario"),
    textInput(ns("user_id"), "ID"),
    textInput(ns("name"), "Nombres"),
    textInput(ns("last_name"), "Apellidos"),
    selectInput(ns("privileges"), "Privilegios", choices = c("user", "admin")),
    dateInput(ns("date_added"), "Fecha", language = "es", value = lubridate::today("America/Lima")),
    btn_agregar(ns("insert_user")),
    h3("Eliminar usuario"),
    uiOutput(ns("select_user")),
    btn_eliminar(ns("delete_user"))
  )
}

#' admin_users UI OUTPUT Functions
#'
#' @noRd
mod_admin_users_output <- function(id){
  ns <- NS(id)
  tagList(
    h3("Tabla de usuarios"),
    tableOutput(ns("table_users"))
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
        date_added = as.character(input$date_added)
      )
    })

    observeEvent(input$insert_user, {

      insert_user(new_user_data())

      updateTextInput(session, "user_id", value = "")
      updateTextInput(session, "name", value = "")
      updateTextInput(session, "last_name", value = "")
      updateSelectInput(session, "privileges", selected = "user")

      vals$data_users <- get_users()

      showModal(modalDialog(
        title = "Nuevo usuario añadido",
        footer = modalButton("Ok")
      ))
    })

    observeEvent(input$delete_user,{

      delete_user(input$del_user_id)

      vals$data_users <- get_users()

      showModal(modalDialog(
        title = "Usuario eliminado",
        footer = modalButton("Ok")
      ))
    })

    output$table_users <- renderTable(vals$data_users)

    output$select_user <- renderUI({
      selectInput(inputId = ns("del_user_id"),
                  label = "Elegir usuario a eliminar",
                  choices = vals$data_users$user_id)
    })

  })
}

## To be copied in the UI
# mod_admin_users_input("admin_users_1")
# mod_admin_users_output("admin_users_1")

## To be copied in the server
# mod_admin_users_server("admin_users_1")
