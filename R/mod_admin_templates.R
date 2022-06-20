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
        col_1(btn_add(ns("add_step"))),
        col_1(btn_trash(ns("rm_step")))
      ),
      textInput(inputId = ns("step_description"),
                label = "Descripción de tarea")
    )
  )
}

#' admin_templates Server Functions
#'
#' @noRd
mod_admin_templates_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

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
    mod_admin_templates_server(id)
  }

  shinyApp(ui, server)
}

## To be copied in the UI
# mod_admin_templates_ui("admin_templates_1")

## To be copied in the server
# mod_admin_templates_server("admin_templates_1")
