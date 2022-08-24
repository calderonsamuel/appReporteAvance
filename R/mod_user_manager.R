#' user_manager UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_user_manager_ui <- function(id) {
    ns <- NS(id)
    tagList(
        btn_agregar(ns("add"), icon = icon("plus"))
    )
}

mod_user_manager_output <- function(id) {
    ns <- NS(id)
    tagList(
        bs4Dash::box(
            title = "Gestión de usuario",
            id = ns("box_manager"),
            width = 12,

            fluidRow(
                col_4(textInput(ns("user_id"), "ID")),
                col_4(textInput(ns("name"), "Nombres")),
                col_4(textInput(ns("last_name"), "Apellidos"))
            ),
            fluidRow(
                col_4(selectInput(ns("privileges"), "Privilegios", choices = c("user1", "user2", "admin"))),
                col_4(selectInput(ns("responds_to"), "Responde a:", choices = user_get_from_privileges("user2"))),
                col_4(dateInput(ns("date_added"), "Fecha", language = "es", value = lubridate::today("America/Lima")))
            ),
            btn_cancelar(ns("cancelar")),
            btn_guardar(ns("save"))
        ) |>
            boxHide()
            # tagAppendAttributes(.cssSelector = glue::glue("#{ns('box_manager')}"), style = "display: none;")
    )
}

#' user_manager Server Functions
#'
#' @noRd
mod_user_manager_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # bs4Dash::updateBox("box_manager", "remove")

        out <- reactiveValues(
            save_sucess = 0
        )

        reset_ui <- function() {
            updateTextInput(session, "user_id", value = "")
            updateTextInput(session, "name", value = "")
            updateTextInput(session, "last_name", value = "")
            updateSelectInput(session, "privileges", selected = "user1")
            updateSelectInput(session, "responds_to", choices = user_get_from_privileges("user2"))
        }

        new_user_data <- reactive({
            data.frame(
                user_id = input$user_id,
                name = input$name,
                last_name = input$last_name,
                privileges = input$privileges,
                responds_to = input$responds_to,
                date_added = as.character(input$date_added)
            )
        })

        observe({
            if (isFalsy(input$user_id, input$name, input$last_name)) {
                alert_error(session, "Debe especificar datos completos de usuario")
            } else {
                user_insert(new_user_data())

                reset_ui()

                out$save_success <- out$save_success + 1

                bs4Dash::updateBox(id = "box_manager", action = "remove")
                alert_info(session = session, sprintf("Se añadió al usuario %s", input$user_id))
            }
        }) |> bindEvent(input$save)

        observe({
            bs4Dash::updateBox(id = "box_manager", action = "restore")
        }) |> bindEvent(input$add)

        observe({
            reset_ui()
            bs4Dash::updateBox(id = "box_manager", action = "remove")

        }) |> bindEvent(input$cancelar)

        # return value
        reactive(out$save_success)

    })
}

## To be copied in the UI
# mod_user_manager_ui("user_manager_1")

## To be copied in the server
# mod_user_manager_server("user_manager_1")


mod_user_manager_apptest <- function() {
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
                bs4Dash::tabItem(
                    tabName = "tasks",
                    mod_user_manager_ui("test"),
                    tags$hr(),
                    mod_user_manager_output("test")
                )
            )
        )
    )

    server <- function(input, output, session) {
        mod_user_manager_server("test")
    }

    shinyApp(ui, server)
}
