#' board UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_board_ui <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            bs4Dash::box(
                title = "Pendiente",
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 3,
                class = "cw cw-lg",
                icon = icon("calendar"),
                uiOutput(ns("pendientes"))
            ),
            col_3(
                bs4Dash::box(
                    title = "En proceso",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    width = 12,
                    class = "cw cw-sm",
                    icon = icon("pen"),
                    uiOutput(ns("en_proceso"))
                ),
                bs4Dash::box(
                    title = "Pausado",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    width = 12,
                    class = "cw cw-sm",
                    icon = icon("pause-circle"),
                    uiOutput(ns("pausado"))
                ),
            ),
            bs4Dash::box(
                title = "En revisión",
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 3,
                class = "cw cw-lg",
                icon = icon("eye"),
                uiOutput(ns("en_revision"))
            ),
            bs4Dash::box(
                title = "Terminado",
                solidHeader = TRUE,
                collapsible = FALSE,
                width = 3,
                class = "cw cw-lg",
                icon = icon("check-circle"),
                uiOutput(ns("terminado"))
            )
        )
    )
}

#' board Server Functions
#'
#' @noRd
mod_board_server <- function(id, AppData) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        output$pendientes <- renderUI({
            AppData$tasks |> 
                purrr::keep(~ .x$status_current == "Pendiente") |> 
                lapply(task_box) |>
                tagList()
        }) 
        
        output$en_proceso <- renderUI({
            AppData$tasks |> 
                purrr::keep(~ .x$status_current == "En proceso") |> 
                lapply(task_box) |>
                tagList()
        })
        
        output$pausado <- renderUI({
            AppData$tasks |> 
                purrr::keep(~ .x$status_current == "Pausado") |> 
                lapply(task_box) |>
                tagList()
        })
        
        output$en_revision <- renderUI({
            AppData$tasks |> 
                purrr::keep(~ .x$status_current == "En revisión") |> 
                lapply(task_box) |>
                tagList()
        })
        
        output$terminado <- renderUI({
            AppData$tasks |> 
                purrr::keep(~ .x$status_current == "Terminado") |> 
                lapply(task_box) |>
                tagList()
        })
        
    })
}
    
## To be copied in the UI
# mod_board_ui("board_1")
    
## To be copied in the server
# mod_board_server("board_1")

mod_board_apptest <- function() {
    AppData <- AppData$new("dgco93@mininter.gob.pe")
    quick_bs4dash(
        modUI = mod_board_ui(id = "test"),
        modServer = mod_board_server(id = "test", AppData)
    )
}
