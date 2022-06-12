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
        uiOutput(ns("pendientes"))
      ),
      bs4Dash::box(
        title = "En proceso",
        solidHeader = TRUE,
        collapsible = FALSE,
        width = 3
      ),
      col_3(
        bs4Dash::box(
          title = "En revisión",
          solidHeader = TRUE,
          collapsible = FALSE,
          width = 12
        ),
        bs4Dash::box(
          title = "Pausado",
          solidHeader = TRUE,
          collapsible = FALSE,
          width = 12
        ),
      ),
      bs4Dash::box(
        title = "Terminado",
        solidHeader = TRUE,
        collapsible = FALSE,
        width = 3
      )
    )
    # wellPanel(
    #   prettyRadioButtons(
    #     inputId = ns("estado"),
    #     label = "Indique estado de avance",
    #     choices = c("Pendiente", "En proceso", "Postergado/Pausado",
    #                 "Esperando revision", "Terminado"),
    #     selected = character()
    #   ),
    #   textAreaInput(
    #     inputId = ns("descripcion_avance"),
    #     label = "Ingrese descripción del avance",
    #     placeholder = "Sea lo más detallado posible"
    #   )
    # )
  )
}

#' progress Server Functions
#'
#' @noRd
mod_progress_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    vals <- reactiveValues(
      user_tasks = get_task_from_user(user = "dgco93")
    )

    tasks_by_status <- reactiveValues(
      pendientes = reactive(subset(vals$user_tasks, status == "Pendiente")),
      en_proceso = reactive(subset(vals$user_tasks, status == "En proceso"))
    )

    output$pendientes <- renderUI({
      # data <- tasks_by_status$pendientes
      # for (row in seq_len(nrow(data))) {
      #   row_data <- data[row,]
      # }
      # tableOutput(ns("tabla_pendientes"))
    })

    output$tabla_pendientes <- renderTable(vals$user_tasks)

  })
}

mod_progress_testapp <- function() {
  ui <- fluidPage(mod_progress_ui("progress_1"))

  server <- function(input, output, session) {
    mod_progress_server("progress_1")
  }

  shinyApp(ui, server)
}

## To be copied in the UI
# mod_progress_ui("progress_1")

## To be copied in the server
# mod_progress_server("progress_1")
