#' groupAdmin UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_groupAdmin_ui <- function(id) {
    ns <- NS(id)
    
    tagList(
        shinyWidgets::actionBttn(
            inputId = ns("modify_group"),
            label = "Administrar grupo",
            size = "md",
            style = "simple",
            color = "primary"
        )
    )
}

#' groupAdmin Server Functions
#'
#' @noRd
mod_groupAdmin_server <- function(id, AppData, config) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        group_selected <- reactive(AppData$group_users[[config$group_selected()]])
        
        observe({
            showModal(modalDialog(
                title = "Panel de grupo",
                size = "l",
                # h3(""),
                DT::DTOutput(ns("debug")),
                
                footer = tagList(
                    modalButton("Cancelar"),
                    btn_guardar(ns("save"))
                )
                    
            ))
        }) |> 
            bindEvent(input$modify_group)
        
        
        output$debug <- DT::renderDT({
            data <- group_selected() |> 
                lapply(\(x) {
                    data.frame(
                        display_name = paste(x$name, x$last_name),
                        picker = color_dropdown(
                            inputId = x$user_id, 
                            color_selected = x$user_color
                        ) |> as.character()
                    )
                })
            
            data <- do.call(rbind, data)
                
            data |>     
                DT::datatable(
                    escape = FALSE, 
                    options = list(
                        dom = "t",
                        ordering = FALSE
                    ), 
                    rownames = FALSE,
                    selection = 'none',
                    colnames = rep("", ncol(data))
                )
        })
    })
}

## To be copied in the UI
# mod_groupAdmin_ui("groupAdmin_1")

## To be copied in the server
# mod_groupAdmin_server("groupAdmin_1")

mod_groupAdmin_apptest <- function(id = "test") {
    
    AppData <- AppData$new()
    config <- fake_config(AppData)
    
    quick_bs4dash(
        modUI = mod_groupAdmin_ui(id),
        modServer = mod_groupAdmin_server(id, AppData, config)
    )
}

color_dropdown <- function(inputId, color_selected) {
    tq <- shinyWidgets::colorSelectorDrop(
        inputId = inputId,
        label = NULL,
        choices = reportes_bs_colors() |> unlist(),
        selected = reportes_bs_colors()[[color_selected]], 
        ncol = 5,
    ) |> htmltools::tagQuery()
    
    tq$
        children("button")$
        addAttrs(style = paste0("outline: none; background-color: ", reportes_bs_colors()[[color_selected]]))$
        allTags()
}

