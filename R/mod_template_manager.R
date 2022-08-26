#' template_manager UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_template_manager_ui <- function(id) {
    ns <- NS(id)
    btn_agregar(ns("add"), icon = icon("plus"))
}

mod_template_manager_output <- function(id, user_iniciado) {
    ns <- NS(id)
    
    group_id <- gruser_get_groups(user_iniciado)
    all_owners <- union(user_iniciado, group_id)
    
    tagList(
        bs4Dash::box(
            title = "Gestión de plantillas",
            width = 12,
            id = ns("box_manager"),
            
            fluidRow(
              col_6(
                  textInput(inputId = ns("temp_description"),
                            label = "Nombre de plantilla",
                            placeholder = "Ingresar nombre de plantilla")
              ),
              col_4(
                  selectInput(
                      inputId = ns("template_owner"),
                      label = "Seleccione dueño de plantilla",
                      choices = setNames(all_owners, all_owners |> purrr::map_chr(user_get_names))
                  )
              ),
              col_2(
                  selectInput(ns("plantilla_mapro"), 
                              label = "¿Es proceso MAPRO?", 
                              choices = c("No", "Sí"))
              )
            ),
            
            conditionalPanel(
                condition = "input.plantilla_mapro == 'Sí'",
                ns = ns,
                textInput(inputId = ns("template_id"),
                          label = "ID de proceso",
                          placeholder = "Solo llenar en procesos MAPRO")
            ),
            h5("Ingresar tareas"),
            fluidRow(
                col_10(
                    uiOutput(ns("step_list")),
                ),
                col_2(
                    btn_add(ns("add_step"), size = "sm"),
                    btn_minus(ns("rm_step"), size = "sm")
                )
                # col_1(btn_add(ns("add_step"), size = "sm")),
                # col_1(btn_minus(ns("rm_step"), size = "sm"))
            ),
            btn_cancelar(ns("cancelar")), 
            btn_guardar(ns("guardar"))
        ) |> boxHide()
    )
}

#' template_manager Server Functions
#'
#' @noRd
mod_template_manager_server <- function(id, user_iniciado) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        step_count <- reactiveVal(1L)
        step_list_numbers <- reactive(seq_len(step_count()))
        
        out <- reactiveValues(
            save_success = 0L
        )
        
        template_id <- reactive({
            if (input$template_id == "") {
                ids::proquint(n_words = 3, use_openssl = TRUE)
            } else {
                input$template_id
            }
        })
        
        new_template_data <- reactive({
            data.frame(
                template_id = template_id(),
                template_description = input$temp_description,
                user_id = input$template_owner
            )
        })
        
        new_template_steps_data <- reactive({
            step_list_numbers() |>
                lapply(function(x) {
                    data.frame(
                        user_id = input$template_owner,
                        template_id = template_id(),
                        step_id = sprintf("step_%02i", x),
                        step_description = input[[sprintf("step_%02i", x)]]
                    )
                }) |>
                (\(x) do.call(what = rbind, args = x))() # ugly but owrks in R 4.1
            # do.call(what = rbind, args = _) # needs R 4.2
        })
        
        observe({
            bs4Dash::updateBox("box_manager", action = "restore", session = session)
        }) |> bindEvent(input$add)
        
        observeEvent(input$add_step, step_count(step_count() + 1L))
        
        observeEvent(input$rm_step, if (step_count() > 1L) step_count(step_count() - 1L))
        
        observeEvent(input$guardar, {
            
            if (isFalsy(c(input$temp_description, input$step_01))) {
                alert_error(session, "Debe indicar descripción de plantilla y actividades")
            } else {
                step_insert(new_template_steps_data())
                template_insert(new_template_data())
                
                bs4Dash::updateBox("box_manager", action = "remove", session = session)
                
                updateTextInput(session = session,
                                inputId = "temp_description",
                                value = "")
                
                updateTextInput(session = session,
                                inputId = "template_id",
                                value = "")
                
                # Resetear todos los inputs de step
                step_list_numbers() |>
                    lapply(function(x) {
                        updateTextInput(session = session,
                                        inputId = sprintf("step_%02i", x),
                                        value = "")
                    })
                
                out$save_success <- out$save_success + 1L
                
                alert_success(session, "Plantilla añadida")
            }
        })
        
        observe({
            bs4Dash::updateBox("box_manager", action = "remove", session = session)
        }) |> bindEvent(input$cancelar)
        
        output$step_list <- renderUI({
            step_list_numbers() |>
                lapply(function(x) {
                    textInput(
                        inputId = ns(sprintf("step_%02i", x)),
                        label = NULL,
                        placeholder = sprintf("Ingresar tarea %02i", x),
                        value = isolate(input[[sprintf("step_%02i", x)]])
                    )
                })
        })
        
        # return value
        reactive(out$save_success)

    })
}

## To be copied in the UI
# mod_template_manager_ui("template_manager_1")

## To be copied in the server
# mod_template_manager_server("template_manager_1")

mod_template_manager_apptest <- function(id = "test") {
    user_iniciado <- "dgco93@mininter.gob.pe"
    ui <- bs4Dash::dashboardPage(
        header = bs4Dash::dashboardHeader(title = "TEST"),
        sidebar = bs4Dash::dashboardSidebar(
            bs4Dash::sidebarMenu(
                bs4Dash::menuItem(text = "Admin Templates",
                                  tabName = "templates",
                                  icon = icon("book"))
            )
        ),
        body = bs4Dash::dashboardBody(
            bs4Dash::tabItem(
                tabName = "templates", 
                mod_template_manager_ui(id),
                mod_template_manager_output(id, user_iniciado)
            )
        )
    )
    
    server <- function(input, output, session) {
        mod_template_manager_server(id, user_iniciado)
    }
    
    shinyApp(ui, server)
}
