#' data_download UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_download_ui <- function(id) {
    ns <- NS(id)
    
    htmltools::a(
        class = "dropdown-item action-button",
        href = "#",
        id = ns("add"),
        fontawesome::fa("fas fa-file-export"),
        "Opciones de descarga"
    )
}

#' data_download Server Functions
#'
#' @noRd
mod_data_download_server <- function(id, app_data) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        observe({
            tryCatch({
                showModal(modalDialog(
                    title = "Descargar datos",
                    size = "l",
                    
                    tags$p(
                        "En esta sección puedes descargar los datos reportados. ",
                        "En todos los casos se presentarán los datos resumidos por Unidad de Medida."
                    ),
                    
                    dateRangeInput(
                        inputId = ns("date_range"),
                        label = "Periodo de datos",
                        start = lubridate::today() - lubridate::days(14),
                        end = lubridate::today(), 
                        max = lubridate::today(), 
                        language = "es",
                        format = "dd/MM/yyyy",
                        separator = " a "
                    ),
                    
                    checkboxInput(
                        inputId = ns("separate_by_report"),
                        label = "¿Desglosar por reporte individual?", 
                        value = TRUE
                    ),
                    
                    checkboxInput(
                        inputId = ns("include_totals"),
                        label = "¿Incluir totales?",
                        value = TRUE
                    ),
                    
                    data_download_tasks(ns("vars_tasks")),
                    data_download_reports(ns("vars_reports")),
                    
                    footer = tagList(
                        modalButton("Cerrar"),
                        btn_download_custom(
                            outputId = ns("download"),
                            label = "Descargar",
                            class = "btn-primary"
                        )
                    )
                ))
            }, error = \(e) showNotification(e, session = session, type = "error"))
        }) |> 
            bindEvent(input$add)
        
        output$download <- downloadHandler(
            filename = function() {
                paste("data-", Sys.time(), ".xlsx", sep="")
            },
            content = function(file) {
                tasks <- iris[sample(seq_len(nrow(iris)), 3),]
                
                reports <- app_data$fetch_reports_to_download(
                    start_date = input$date_range[1],
                    end_date = input$date_range[2]
                ) |> 
                    data_download_handle_reports_data(
                        input$include_totals, 
                        input$separate_by_report
                    )
                
                metadata <- tibble::tribble(
                    ~Campo, ~Valor,
                    "Hora de consulta", as.character(lubridate::now("America/Lima")),
                    "Inicio de periodo", as.character(input$date_range[1]),
                    "Fin de periodo", as.character(input$date_range[2]),
                    "Columnas de tareas", paste(input$vars_tasks, collapse = "; "),
                    "Columnas de tareas", paste(input$vars_reports, collapse = "; ")
                )
                    
                writexl::write_xlsx(
                    x = list(
                        Tareas = tasks,
                        Reportes = reports,
                        Metadata = metadata
                    ), 
                    path = file
                )
            }
        )
        
    })
}

## To be copied in the UI
# mod_data_download_ui("data_download_1")

## To be copied in the server
# mod_data_download_server("data_download_1")

data_download_tasks <- function(inputId) {
    choices <- c(
        "Iniciadas",
        "Terminadas 1 (incluye previas)",
        "Terminadas 2 (solo del periodo)"
    )
    
    shinyWidgets::checkboxGroupButtons(
        inputId = inputId,
        label = "Columnas de Tareas",
        choices = choices,
        selected = choices[2],
        individual = TRUE,
        checkIcon = list(
            yes = tags$i(class = "fa fa-circle",
                         style = "color: steelblue"),
            no = tags$i(class = "fa fa-circle-o",
                        style = "color: steelblue")
        )
    )
}

data_download_reports <- function(inputId) {
    choices <- c(
        "Terminadas 2 (solo del periodo)"
    )
    
    shinyWidgets::checkboxGroupButtons(
        inputId = inputId,
        label = "Columnas de Reportes",
        choices = choices,
        selected = choices[1],
        individual = TRUE,
        checkIcon = list(
            yes = tags$i(class = "fa fa-circle",
                         style = "color: steelblue"),
            no = tags$i(class = "fa fa-circle-o",
                        style = "color: steelblue")
        ),
        disabled = TRUE
    )
}

data_download_handle_reports_data <- function(reports_data, use_totals = TRUE, separate_by_report = TRUE) {
    processed <- reports_data |> 
        dplyr::group_by(output_unit, report_title) |> 
        dplyr::summarise(
            sum_output = sum(output_progress, na.rm = TRUE)
        ) |> 
        dplyr::ungroup() |> 
        tidyr::pivot_wider(
            names_from = report_title,
            values_from = sum_output, 
            values_fill = 0
        ) |> 
        dplyr::rowwise() |> 
        dplyr::mutate(
            `Total Reportado` = sum(dplyr::c_across(-output_unit))
        ) |> 
        dplyr::rename(`Unidad de medida` = output_unit) |> 
        dplyr::ungroup()
    
    if (use_totals) {
        col_totals <- processed |> 
            dplyr::summarise(
                dplyr::across(dplyr::where(is.numeric), sum),
                dplyr::across(dplyr::where( \(x) !is.numeric(x)), \(x) "Total de columna")
            )
        
        processed <- processed |> 
            dplyr::bind_rows(col_totals)
    }
    
    if (!separate_by_report) {
        processed <- processed |> 
            dplyr::select(`Unidad de medida`, `Total Reportado`)
    }
    
    processed
}
