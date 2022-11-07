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
                    icon = fontawesome::fa("fas fa-ellipsis-h"),
                    bs4Dash::boxDropdownItem(
                        "Agregar",
                        icon = fontawesome::fa("fas fa-plus-circle"),
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
        )
    )
}

#' board Server Functions
#'
#' @noRd
mod_board_server <- function(id, AppData) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        observe({
            showModal(modalDialog(
                h1("Añadir tarea"),
                selectInput(
                    inputId = ns("org_id"), 
                    label = "Organización", 
                    choices = head(letters)),
                selectInput(
                    inputId = ns("group_id"), 
                    label = "Equipo", 
                    choices = tail(letters)),
                textInput(
                    inputId = ns("title"), 
                    label = "Título de tarea"
                ),
                textAreaInput(
                    inputId = ns("description"), 
                    label = "Descripción de tarea"
                ),
                shinyWidgets::airDatepickerInput(
                    inputId = "time_due",
                    label = "Plazo máximo",
                    value = lubridate::now("America/Lima"), 
                    timepicker = TRUE,
                    dateFormat = "dd/mm/yyyy", 
                    language = "es",
                    minDate = lubridate::today("America/Lima"),
                    maxDate = lubridate::today("America/Lima") + lubridate::weeks(4),
                    todayButton = TRUE,
                    timepickerOpts = shinyWidgets::timepickerOptions(
                        minutesStep = 15,
                        minHours = 8,
                        maxHours = 17
                    )
                ),
                selectInput(
                    inputId = ns("output_unit"), 
                    label = "Unidad de medida",
                    choices = c("Documento")
                ),
                numericInput(
                    inputId = ns("output_goal"),
                    label = "Meta",
                    value = 1
                )
                
                
            ))
        }) |> bindEvent(input$task_add)
        
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
    id = ids::random_id()
    quick_bs4dash(
        modUI = mod_board_ui(id = id),
        modServer = mod_board_server(id = id, AppData)
    )
}
