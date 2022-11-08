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
                headerBorder = FALSE,
                class = "cw cw-lg",
                icon = icon("calendar"),
                dropdownMenu = bs4Dash::boxDropdown(
                    icon = fontawesome::fa("fas fa-ellipsis"),
                    bs4Dash::boxDropdownItem(
                        "Agregar",
                        icon = fontawesome::fa("fas fa-circle-plus"),
                        id = ns("task_add")
                    )
                ),
                uiOutput(ns("pendientes"))
            ),
            col_3(
                bs4Dash::box(
                    title = "En proceso",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    headerBorder = FALSE,
                    width = 12,
                    class = "cw cw-sm",
                    icon = icon("pen"),
                    uiOutput(ns("en_proceso"))
                ),
                bs4Dash::box(
                    title = "Pausado",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    headerBorder = FALSE,
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
                headerBorder = FALSE,
                width = 3,
                class = "cw cw-lg",
                icon = icon("eye"),
                uiOutput(ns("en_revision"))
            ),
            bs4Dash::box(
                title = "Terminado",
                solidHeader = TRUE,
                collapsible = FALSE,
                headerBorder = FALSE,
                width = 3,
                class = "cw cw-lg",
                icon = icon("check-circle"),
                uiOutput(ns("terminado"))
            )
        ),
        
        
        mod_task_add_ui(ns("task_add_1"))
    )
}

#' board Server Functions
#'
#' @noRd
mod_board_server <- function(id, AppData) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        # Modules ----
        
        task_gets_added <- mod_task_add_server(
            id = "task_add_1", 
            AppData = AppData, 
            trigger = reactive(input$task_add)
        )
        
        # Reactives ----
        
        tasks <- reactive({
            print(task_gets_added())
            print(paste0("N: ", length(AppData$tasks)))
            AppData$tasks
        }) |> bindEvent(task_gets_added())
        
        # Outputs ----
        
        output$pendientes <- renderUI({
            tasks() |> 
                purrr::keep(~ .x$status_current == "Pendiente") |> 
                lapply(task_box) |>
                tagList()
        }) 
        
        output$en_proceso <- renderUI({
            tasks() |> 
                purrr::keep(~ .x$status_current == "En proceso") |> 
                lapply(task_box) |>
                tagList()
        })
        
        output$pausado <- renderUI({
            tasks() |> 
                purrr::keep(~ .x$status_current == "Pausado") |> 
                lapply(task_box) |>
                tagList()
        })
        
        output$en_revision <- renderUI({
            tasks() |> 
                purrr::keep(~ .x$status_current == "En revisión") |> 
                lapply(task_box) |>
                tagList()
        })
        
        output$terminado <- renderUI({
            tasks() |> 
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
    id = ids::random_id()
    quick_bs4dash(
        modUI = mod_board_ui(id = id),
        modServer = mod_board_server(id = id, AppData)
    )
}
