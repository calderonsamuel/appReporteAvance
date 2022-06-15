#' admin UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_admin_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_4(
        bs4Dash::box(
          width = 12,
          mod_admin_users_input(ns("admin_users_1"))
        )
      ),
      col_8(
        bs4Dash::box(
          width = 12,
          mod_admin_users_output(ns("admin_users_1"))
        )
      )
    )
  )
}

#' admin Server Functions
#'
#' @noRd
mod_admin_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    mod_admin_users_server("admin_users_1")

  })
}

mod_admin_testapp <- function() {

  ui <- fluidPage(mod_admin_ui("test"))

  server <- function(input, output, session) {
    mod_admin_server("test")
  }

  shinyApp(ui, server)
}

## To be copied in the UI
# mod_admin_ui("admin_1")

## To be copied in the server
# mod_admin_server("admin_1")
