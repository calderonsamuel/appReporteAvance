#' admin UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_admin_ui <- function(id, privileges){
  ns <- NS(id)
  if (privileges != "admin") return(NULL) else {
    bs4Dash::tabItem(
      tabName = "admin-users",
      mod_admin_users_ui(ns("admin_users_1"))
    )
  }
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

mod_admin_sidebar <- function(id, privileges) {
  ns <- NS(id)
  if (privileges != "admin") return(NULL)
  tagList(
      bs4Dash::menuSubItem(
        text = "Usuarios",
        tabName = "admin-users"
      )
  )
}

mod_admin_testapp <- function(id = "mod_admin_1", privileges = "admin") {

  # ui <- fluidPage(mod_admin_ui("test title"))
  ui <- bs4Dash::dashboardPage(
    header = bs4Dash::dashboardHeader(title = "test"),
    sidebar = bs4Dash::dashboardSidebar(
      bs4Dash::sidebarMenu(
        bs4Dash::menuItem(
          text = "test menu",
          mod_admin_sidebar(id, privileges = privileges)
        )
      )
    ),
    body = bs4Dash::dashboardBody(
      bs4Dash::tabItems(
        mod_admin_ui(id, privileges = privileges)
      )
    )
  )

  server <- function(input, output, session) {
    mod_admin_server(id)
  }

  shinyApp(ui, server)
}

## To be copied in the UI
# mod_admin_ui("admin_1")

## To be copied in the server
# mod_admin_server("admin_1")
