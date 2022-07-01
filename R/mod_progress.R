#' progress UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList radioButtons textAreaInput wellPanel
mod_progress_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Dash::box(
        title = "Pendiente",
        solidHeader = TRUE,
        collapsible = FALSE,
        width = 3,
        icon = icon("calendar"),
        uiOutput(ns("pendientes"))
      ),
      col_3(
        bs4Dash::box(
          title = "En proceso",
          solidHeader = TRUE,
          collapsible = FALSE,
          width = 12,
          icon = icon("pen")
        ),
        bs4Dash::box(
          title = "Pausado",
          solidHeader = TRUE,
          collapsible = FALSE,
          width = 12,
          icon = icon("pause-circle")
        ),
      ),
      bs4Dash::box(
        title = "En revisión",
        solidHeader = TRUE,
        collapsible = FALSE,
        width = 3,
        icon = icon("eye")
      ),
      bs4Dash::box(
        title = "Terminado",
        solidHeader = TRUE,
        collapsible = FALSE,
        width = 3,
        icon = icon("check-circle")
      )
    )
  )
}

#' progress Server Functions
#'
#' @noRd
mod_progress_server <- function(id, user_iniciado){
  moduleServer( id, function(input, output, session){
    ns <- session$ns



    vals <- reactiveValues(
      groups = reactive(get_groups_from_user(user_iniciado())$group_id)
      # pendientes_user = reactive(get_tasks_pendientes(user = user_iniciado())),
      # pendientes_group = reactive(get_tasks_pendientes(user = ))
    )

    pendientes <- reactiveValues(
      user = reactive(get_tasks_pendientes(user = user_iniciado())),
      group = reactive(get_tasks_pendientes(user = vals$groups()))
    )

    output$pendientes <- renderUI({
      tagList(
        pendientes$user()$task_description |>
          lapply(box_pendientes_user),
        pendientes$group()$task_description |>
          lapply(box_pendientes_group)
      )

    })

  })
}

mod_progress_testapp <- function(id = "test") {
  ui <- bs4Dash::dashboardPage(
    header = bs4Dash::dashboardHeader(title = "TEST"),
    sidebar = bs4Dash::dashboardSidebar(
      bs4Dash::sidebarMenu(
        bs4Dash::menuItem(text = "Reporte",
                          tabName = "progress",
                          icon = icon("tasks"))
      )
    ),
    body = bs4Dash::dashboardBody(
      bs4Dash::tabItem(tabName = "progress", mod_progress_ui(id))
    )
  )

  server <- function(input, output, session) {
    user_iniciado <- reactive("dgco93@mininter.gob.pe")
    mod_progress_server(id, user_iniciado)
  }

  shinyApp(ui, server)
}

# mod_progress_testapp <- function() {
#   ui <- fluidPage(mod_progress_ui("progress_1"))
#
#   server <- function(input, output, session) {
#     user_iniciado <- reactive("dgco93@mininter.gob.pe")
#     mod_progress_server("progress_1", user_iniciado)
#   }
#
#   shinyApp(ui, server)
# }

## To be copied in the UI
# mod_progress_ui("progress_1")

## To be copied in the server
# mod_progress_server("progress_1")
