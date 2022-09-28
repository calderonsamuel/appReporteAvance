#' user_manager UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_user_manager_btns <- function(id) {
    ns <- NS(id)
    tagList(
        btn_agregar(ns("add"), icon = icon("plus")),
        btn_editar(ns("edit"), icon = icon("user-edit"))
    )
}

mod_user_manager_inputs <- function(id) {
    ns <- NS(id)
    tagList(
        boxHidden(
            title = "Gestión de usuario",
            id = ns("box_manager"),
            width = 12,

            fluidRow(
                col_4(textInput(ns("user_id"), "ID"), id = ns("user_id_input")),
                col_4(textInput(ns("name"), "Nombres")),
                col_4(textInput(ns("last_name"), "Apellidos"))
            ),
            fluidRow(
                col_4(dateInput(ns("date_added"), "Fecha", language = "es", value = lubridate::today("America/Lima")))
            ),
            btn_cancelar(ns("cancelar")),
            btn_modificar(ns("modify")),
            btn_guardar(ns("save")) 
        )
    )
}

#' user_manager Server Functions
#'
#' @noRd
mod_user_manager_server <- function(id, SessionData, selected_user) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        out <- reactiveValues(
            save_or_modify = 0
        )

        reset_ui <- function() {
            updateTextInput(session, "user_id", value = "")
            updateTextInput(session, "name", value = "")
            updateTextInput(session, "last_name", value = "")
        }
        
        toggle_inputs <- function(state = "hide") {
            match.arg(state, c("hide", "show"))
            
            inputs <- c("user_id_input", "date_added")
            state_function <- if (state == "hide") shinyjs::hide else shinyjs::show
            purrr::walk(inputs, state_function)
        }
        
        new_user_data <- reactive({
            data.frame(
                user_id = input$user_id,
                name = input$name,
                last_name = input$last_name,
                date_added = as.character(input$date_added)
            )
        })
        
        observe({
            if (isFalsy(input$user_id, input$name, input$last_name)) {
                alert_error(session, "Debe especificar datos completos de usuario")
            } else {
                SessionData$user_insert(new_user_data())
                
                out$save_or_modify <- out$save_or_modify + 1

                bs4Dash::updateBox(id = "box_manager", action = "remove", session = session)
                alert_info(session = session, sprintf("Se añadió al usuario %s", input$user_id))
            }
        }) |> bindEvent(input$save)
        
        observe({
            tryCatch({
                SessionData$user_update(selected_user(), input$name, input$last_name)
                bs4Dash::updateBox(id = "box_manager", action = "remove", session = session)
                out$save_or_modify <- out$save_or_modify + 1
                alert_info(session = session, sprintf("Se modificó el usuario %s", input$user_id))
            }, error = \(e) alert_error(session, e))
        }) |> bindEvent(input$modify)

        observe({
            shinyjs::show("save")
            shinyjs::hide("modify")
            reset_ui()
            toggle_inputs(state = "show")
            
            bs4Dash::updateBox(id = "box_manager", action = "restore", session = session)
        }) |> bindEvent(input$add)

        observe({
            if (!isTruthy(selected_user())) {
                alert_error(session, "Debe seleccionar un usuario a modificar")
            } else {
                shinyjs::show("modify")
                shinyjs::hide("save")
                reset_ui()
                toggle_inputs(state = "hide")
                
                bs4Dash::updateBox(id = "box_manager", action = "restore", session = session)
            }
        }) |> bindEvent(input$edit)

        observe({
            bs4Dash::updateBox(id = "box_manager", action = "remove", session = session)

        }) |> bindEvent(input$cancelar)

        # return value
        reactive(out$save_or_modify)

    })
}

## To be copied in the UI
# mod_user_manager_btns("user_manager_1")

## To be copied in the server
# mod_user_manager_server("user_manager_1")


mod_user_manager_apptest <- function() {
    session_data <- SessionData$new("dgco93@mininter.gob.pe")
    ui <- tagList(
        bs4Dash::dashboardPage(
            header = bs4Dash::dashboardHeader(title = "TEST"),
            sidebar = bs4Dash::dashboardSidebar(
                bs4Dash::sidebarMenu(
                    bs4Dash::menuItem(text = "Test",
                                      tabName = "tasks",
                                      icon = icon("user"))
                )
            ),
            body = bs4Dash::dashboardBody(
                shinyjs::useShinyjs(),
                bs4Dash::tabItem(
                    tabName = "tasks",
                    mod_user_manager_btns("test"),
                    tags$hr(),
                    mod_user_manager_inputs("test")
                )
            )
        )
    )

    server <- function(input, output, session) {
        selected_user <- reactive("dgco93@mininter.gob.pe")
        mod_user_manager_server("test", session_data, selected_user)
    }

    shinyApp(ui, server)
}
