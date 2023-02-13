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
        btn_user_add(ns("add")),
        uiOutput(ns("users")),
        DT::DTOutput(ns("tabla"), width = "350"),
    )
}

#' groupAdmin Server Functions
#'
#' @noRd
mod_groupAdmin_server <- function(id, AppData, config) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        group_selected <- reactive(AppData$group_users[[config$group_selected()]])
        
        output$users <- renderUI({
            group_selected() |> 
                lapply(\(x) {
                    fluidRow(
                        class = "py-1",
                        style = "max-width: 350px",
                        div(
                            class = "col-md-auto d-flex align-items-center",
                            span(class = paste0("badge user-color-badge px-3 bg-", x$user_color), " ")
                        ),
                        div(
                            class = "col d-flex align-items-center",
                            paste(x$name, x$last_name)
                        ),
                        div(
                            class = "col d-flex align-items-center",
                            admin_toolbar(x$user_id, ns, "userToEdit", "userToDelete")
                        )
                    )
                })
        })
        
        output$tabla <- DT::renderDT({
            data <- group_selected() |> 
                lapply(\(x) {
                    data.frame(
                        colors = span(class = paste0("badge user-color-badge px-3 mr-1 bg-", x$user_color), " ") |> as.character(),
                        user = paste(x$name, x$last_name),
                        btns = span(
                            actionButton(paste0(x$user_id, "-edit"), label = fontawesome::fa("fas fa-pencil")),
                            actionButton(paste0(x$user_id, "-delete"), label = fontawesome::fa("fas fa-trash"))
                        ) |> as.character()
                    )
                })
            
            data <- do.call(rbind, data)
                
            data |>     
                DT::datatable(
                    escape = FALSE,  
                    options = list(
                        dom = "t",
                        ordering = FALSE,
                        columnDefs = list(
                            list(
                                className = 'dt-right', 
                                targets = 2
                            ),
                            list(className = "dt-left",
                                 targets = 0:1)
                        )
                    ), 
                    rownames = FALSE,
                    selection = 'none',
                    # style = "bootstrap4",
                    colnames = rep("", ncol(data))
                )
        })
        
        observe({
            showModal(modalDialog(
                title = "Nuevo miembro",
                size = "l",
                
                fluidRow(
                    col_6(
                        selectInput(
                            inputId = ns("user"), 
                            label = "Selecciona usuario", 
                            choices = head(letters),
                            width = "100%"
                        )
                    ),
                    col_2(
                        colourpicker::colourInput(
                            ns("color"), 
                            "Color", 
                            palette = "limited", 
                            allowedCols = reportes_bs_colors() |> unlist(), 
                            value = "#d2d6de",
                            showColour = "background",
                            closeOnClick = TRUE
                        )
                    ),
                    col_4(
                        selectInput(
                            inputId = ns("role"),
                            label = "Rol",
                            choices = c(Usuario = "user", Responsable = "admin")
                        )
                    )
                ),
                
                footer = tagList(
                    modalButton("Cancelar"),
                    btn_guardar(ns("save"))
                )
            ))
        }) |> 
            bindEvent(input$add)
        
        
        
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

