#' group_units UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_group_units_ui <- function(id) {
    ns <- NS(id)
    tagList(
        h5("Gestión de mediciones"),
        btn_custom(
          inputId = ns("add"),
          label = "Agregar",
          icon = fontawesome::fa("fas fa-plus"),
          class = "btn-success btn-sm mb-2"
        ),
        uiOutput(ns("units"))
    )
}

#' group_units Server Functions
#'
#' @noRd
mod_group_units_server <- function(id, AppData, group_selection) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        rv <- reactiveValues(
            unit_added = 0L,
            unit_deleted = 0L,
            unit_edited = 0L
        )
        
        group_units <- reactive({
            AppData$group_units
        }) |> 
            bindEvent(
                rv$unit_added,
                rv$unit_deleted,
                rv$unit_edited,
                group_selection$group_selected()
            )
        
        output$units <- renderUI({
            group_units() |> 
                lapply(\(x) {
                    div(
                        class = "row p-1 mx-0 mb-2 mw-100",
                        style = "background-color: #FFFFFF33; border-radius: 5px;",
                        div(
                            class = "col-xs-auto d-flex align-items-center",
                            span(
                                fontawesome::fa(x$icon),
                            )
                        ),
                        div(
                            class = "col d-flex align-items-center mx-2",
                            `data-toggle`= "tooltip",
                            `data-placement`= "top",
                            title = paste0("Descripción: ", x$unit_description),
                            x$unit_title,
                            badge_unit_type(x$type)
                        ),
                        div(
                            class = "col-xs-auto d-flex align-items-center",
                            admin_toolbar(
                                editInputId = ns("unitToEdit"),
                                deleteInputId = ns("unitToDelete"),
                                value = x$unit_id
                            )
                        )
                    )
                })
        })
        
        observe({
            showModal(modalDialog(
                title = "Nueva unidad de medida",
                
                textInput(ns("title"), "Título de unidad de medida", width = "100%"),
                textAreaInput(ns("description"), "Descripción", width = "100%"),
                fluidRow(
                    col_6(
                        selectInput(ns("type"), "Destino", c(Tarea = "task", Reporte = "report"))
                    ),
                    col_6(
                        selectInput(ns("icon"), "Ícono", c("file", "user")),
                    )
                ),
                
                footer = tagList(
                    modalButton("Cancelar"),
                    btn_guardar(ns("save"))
                )
            ))
        }) |> 
            bindEvent(input$add)
        
        observe({
            tryCatch({
                AppData$group_unit_add(
                    unit_title = input$title,
                    unit_description = input$description,
                    unit_type = input$type,
                    unit_icon = input$icon
                )
                
                removeModal(session)
                
                rv$unit_added <- rv$unit_added + 1L
                
                alert_info(session, "Unidad de medida añadida")
            }, error = \(e) alert_error(session, e))
        }) |> 
            bindEvent(input$save)
        
        ## Deletion ----
        observe({
            tryCatch({
                shinyWidgets::ask_confirmation(
                    inputId = ns("confirm_delete"),
                    title = "Eliminar unidad de medida",
                    text = "El equipo ya no tendrá acceso a la unidad de medida",
                    type = "warning",
                    btn_labels = c("Cancelar", "Confirmar"),
                    btn_colors = c("#6e7d88", "#ff5964")
                )
            }, error = \(e) alert_error(session, e))
        }) |> 
            bindEvent(input$unitToDelete)
        
        observe({
            tryCatch({
                if(isTRUE(input$confirm_delete)) {
                    AppData$group_unit_delete(
                        unit_id = input$unitToDelete
                    )
                    
                    rv$unit_deleted <- rv$unit_deleted + 1L
                    
                    alert_info(session, "Unidad de medida eliminada")
                }
                
            }, error = \(e) alert_error(session, e))
        }) |> 
            bindEvent(input$confirm_delete)
        
        ## Edition ----
        observe({
            
            unit_selected <- group_units()[[input$unitToEdit]]
            
            showModal(modalDialog(
                title = "Nueva unidad de medida",
                
                textInput(ns("title"), "Título de unidad de medida", value = unit_selected$unit_title, width = "100%"),
                textAreaInput(ns("description"), "Descripción", value = unit_selected$unit_description, width = "100%"),
                fluidRow(
                    col_6(
                        selectInput(ns("type"), "Destino", c(Tarea = "task", Reporte = "report"), selected = unit_selected$type)
                    ),
                    col_6(
                        selectInput(ns("icon"), "Ícono", c("file", "user"), selected = unit_selected$icon),
                    )
                ),
                
                footer = tagList(
                    modalButton("Cancelar"),
                    btn_guardar(ns("save_edition"))
                )
            ))
            
        }) |> 
            bindEvent(input$unitToEdit)
        
        observe({
            tryCatch({
                AppData$group_unit_edit(
                    unit_id = input$unitToEdit,
                    unit_title = input$title,
                    unit_description = input$description,
                    unit_type = input$type,
                    unit_icon = input$icon
                )
                
                removeModal(session)
                
                rv$unit_edited <- rv$unit_edited + 1L
                
                alert_info(session, "Unidad de medida editada")
                
            }, error = \(e) alert_error(session, e))
        }) |> 
            bindEvent(input$save_edition)
        
    })
}

## To be copied in the UI
# mod_group_units_ui("group_units_1")

## To be copied in the server
# mod_group_units_server("group_units_1")

mod_group_units_apptest <- function(id = "test") {
    
    AppData <- AppData$new()
    
    quick_bs4dash(
        modUI = mod_group_units_ui(id),
        modServer = mod_group_units_server(id, AppData)
    )
}

badge_unit_type <- function(type) {
    bg_class <- switch (type,
        task = "bg-info",
        report = "bg-lime"
    )
    
    type_translate <- switch (type,
        task = "Tarea",
        report = "Reporte"
    )
    
    span(
        class = "badge px-1 bg-info mx-2",
        class = bg_class,
        type_translate # content
    )
}
