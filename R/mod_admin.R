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
  # if (privileges != "admin") return(tagList()) else {
    bs4Dash::tabItem(
      tabName = "admin-users",
      mod_admin_users_ui(ns("admin_users_1"))
    )
  # }
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

mod_admin_menuItem <- function(id, privileges) {
  ns <- NS(id)
  if (privileges != "admin") return(NULL)
  # tagList(
    bs4Dash::menuItem(
      text = "Admin",
      icon = icon("user-shield"),
      bs4Dash::menuSubItem(
        text = "Usuarios",
        tabName = "admin-users"
      )
    )
  # )
}

mod_admin_testapp <- function(id = "mod_admin_1", privileges = "admin") {

  ui <- bs4Dash::dashboardPage(
    header = bs4Dash::dashboardHeader(title = "test"),
    sidebar = bs4Dash::dashboardSidebar(
      bs4Dash::sidebarMenu(
        mod_admin_menuItem(id, privileges)
      )
    ),
    body = bs4Dash::dashboardBody(
      bs4Dash::tabItems(
        mod_admin_ui(id)
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
