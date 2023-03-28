#' report_add UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_report_add_ui <- function(id) {
    ns <- NS(id)
    tagList(
        htmltools::a(
            class = "dropdown-item action-button",
            href = "#",
            id = ns("add"),
            fontawesome::fa("fas fa-file-circle-check"),
            "Nuevo reporte"
        )
    )
}

#' report_add Server Functions
#'
#' @noRd
mod_report_add_server <- function(id, app_data, controlbar) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        module_output <- reactiveValues(
            added = 0L
        )

        processes_choices <- reactive({
            all_processes = controlbar$processes()
            values <- purrr::map_chr(all_processes, "process_id")
            names <- purrr::map_chr(all_processes, "title")

            setNames(values, names)
        })
        
        unit_choices <- reactive({
            app_data$fetch_units(input$process) |> 
                purrr::keep(~.x$type == "report") |> 
                purrr::map_chr("unit_title") |>
                unname()
        })
        
        ## counter for forms ----
        
        counter <- reactiveVal(value = 1L, label = "counter")
        
        observe({
            counter(counter() + 1)
        }) |> bindEvent(input$unit_add)
        
        observe({
            if (counter() > 1L) {
                counter(counter() - 1)
            }
        }) |> bindEvent(input$unit_delete)
        
        ## input_names ----
        
        input_names <- reactive({
            num_seq <- seq_len(counter())
            
            list(
                unit = paste0("form_", num_seq, "_unit"),
                quantity = paste0("form_", num_seq, "_quantity")
            )
        })
        
        
        observe({
            
            counter(1L)
            
            showModal(modalDialog(
                title = "Nuevo reporte",
                size = "l",
                
                textInputPro(
                    inputId = ns("title"), 
                    label = "Título de reporte",
                    width = "100%",
                    maxlength = 125,
                    maxlengthCounter = TRUE
                ),
                textAreaInputPro(
                    inputId = ns("details"), 
                    label = "Detalles", 
                    placeholder = "Opcional",
                    width = "100%",
                    maxlength = 500,
                    maxlengthCounter = TRUE
                ),

                selectInput(
                    inputId = ns("process"),
                    label = "Proceso",
                    choices = processes_choices()
                ),

                
                div(tags$label(class = "control-label", "Unidades de medición")),
                
                btn_custom(
                    inputId = ns("unit_add"),
                    label = "Añadir",
                    icon = fontawesome::fa("fas fa-plus"),
                    class = "btn-success btn-sm mb-2"
                ),
                btn_custom(
                    inputId = ns("unit_delete"),
                    label = "Remover último",
                    icon = fontawesome::fa("fas fa-minus"),
                    class = "btn-danger btn-sm mb-2"
                ),
                
                uiOutput(ns("form")),
                
                footer = tagList(
                    modalButton("Cancelar"),
                    btn_guardar(ns("save"))
                )
            ))
        }) |> 
            bindEvent(input$add)
        
        output$form <- renderUI({
            purrr::map2(input_names()$unit, input_names()$quantity, ~{
                fluidRow(
                    col_8(
                        selector_units(
                            inputId = ns(.x),
                            choices = unit_choices(),
                            selected = isolate(input[[.x]]),
                            width = "100%"
                        )
                    ),
                    col_4(
                        numericInput(
                            inputId = ns(.y),
                            label = NULL,
                            value = input[[.y]] %||% 0L,
                            min = 0,
                            width = "100%"
                        )
                    )
                )
            })
        })
        
        observe({
            tryCatch({
                units <- purrr::map_chr(input_names()$unit, ~input[[.x]])
                quantities <- purrr::map_dbl(input_names()$quantity, ~input[[.x]])
                
                app_data$report_add(
                    report_title = input$title,
                    details = input$details,
                    units = units,
                    quantities = quantities
                )
                
                removeModal(session)
                module_output$added <- module_output$added + 1L
                showNotification("Reporte agregado", duration = 3, 
                                 type = "message", session = session)
                
            }, error = \(e) alert_error(session, e))
        }) |> 
            bindEvent(input$save)

        # update unit choices ----
        observe({
            shinyWidgets::updatePickerInput(
                session = session,
                inputId = "output_unit",
                choices = unit_choices()
            )
        }) |>
            bindEvent(input$process)
        
        # output ----
        
        reactive(module_output$added)
    })
}

## To be copied in the UI
# mod_report_add_ui("report_add_1")

## To be copied in the server
# mod_report_add_server("report_add_1")

selector_units <- function(inputId, choices, selected, width = NULL) {
    shinyWidgets::pickerInput(
        inputId = inputId,
        label = NULL,
        width = width,
        choices = choices,
        selected = selected,
        options = shinyWidgets::pickerOptions(
            liveSearch = TRUE
        )
    )
}
