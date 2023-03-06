#' group_users UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_group_users_ui <- function(id) {
    ns <- NS(id)
    
    tagList(
        h5("Gestión de usuarios"),
        tags$button(
            id = ns("add"), 
            type = "button",
            class = "btn btn-success btn-sm mb-2 action-button", 
            `data-val` = shiny::restoreInput(ns("add"), NULL),
            list(
                fontawesome::fa("fas fa-user-plus"),
                "Agregar"
            )
        ),
        uiOutput(ns("users"))
    )
}

#' group_users Server Functions
#'
#' @noRd
mod_group_users_server <- function(id, AppData, controlbar) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        
        rv <- reactiveValues(
            user_added = 0L,
            user_deleted = 0L,
            user_edited = 0L
        )
        
        group_selected <- reactive(AppData$group_users[[controlbar$group_selected()]]) |> 
            bindEvent(
                rv$user_added,
                rv$user_deleted,
                rv$user_edited,
                controlbar$group_selected()
            )
        
        module_output <- reactiveValues(
            group_colors_modified = 0L
        )
        
        org_users <- reactive({
            org_users_list <- AppData$org_users[[controlbar$org_selected()]]
            current_group_users_ids <- purrr::map_chr(group_selected(), "user_id")
            
            remaining_users <- org_users_list |> 
                purrr::discard(\(x) x$user_id %in% current_group_users_ids) 
            
            ids <- purrr::map_chr(remaining_users, "user_id")
            names <- purrr::map_chr(remaining_users, \(x) paste(x$name, x$last_name))
            
            setNames(ids, names)
        })
        
        output$users <- renderUI({
            group_selected() |> 
                lapply(\(x) {
                    fluidRow(
                        class = "p-1 mx-0 mb-2 mw-100",
                        style = "min-width: 300px; background-color: rgb(255 255 255 / 20%); border-radius: 5px;",
                        div(
                            class = "col-xs-auto d-flex align-items-center",
                            span(class = paste0("badge user-color-badge px-3 bg-", x$user_color), " ")
                        ),
                        div(
                            class = "col d-flex align-items-center",
                            paste(x$name, x$last_name)
                        ),
                        div(
                            class = "col-xs-auto d-flex align-items-center",
                            admin_toolbar(
                                editInputId = ns("userToEdit"),
                                deleteInputId = ns("userToDelete"),
                                value = x$user_id
                            )
                        )
                    )
                })
        })
        
        ## Adding user ----
        observe({
            showModal(modalDialog(
                title = "Nuevo miembro",
                size = "l",
                
                fluidRow(
                    col_6(
                        selectInput(
                            inputId = ns("user"), 
                            label = "Selecciona usuario", 
                            choices = org_users(),
                            width = "100%"
                        )
                    ),
                    col_2(
                        colourpicker::colourInput(
                            ns("color"), 
                            "Color", 
                            palette = "limited", 
                            allowedCols = reportes_bs_colors() |> unlist(), 
                            value = "#D2D6DE",
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
        
        
        observe({
            tryCatch({
                color_selected <- reportes_bs_colors()[reportes_bs_colors() == input$color] |> names()
                
                AppData$group_user_add(
                    group_id = controlbar$group_selected(),
                    user_id = input$user,
                    user_color = color_selected,
                    group_role = input$role
                )
                
                removeModal(session)
                
                rv$user_added <- rv$user_added + 1L
                
                alert_info(session, "Usuario añadido")
                
            }, error = \(e) alert_error(session, e))
        }) |> 
            bindEvent(input$save)
        
        # Deleting user ----
        
        observe({
            tryCatch({
                shinyWidgets::ask_confirmation(
                    inputId = ns("confirm_delete"),
                    title = "Eliminar usuario", 
                    text = "El usuario dejará de pertenecer a este equipo", 
                    type = "warning", 
                    btn_labels = c("Cancelar", "Confirmar"),
                    btn_colors = c("#6e7d88", "#ff5964")
                )
            }, error = \(e) alert_error(session, e))
        }) |> 
            ## input$userToDelete ----
            bindEvent(input$userToDelete) 
        
        observe({
            tryCatch({
                if(isTRUE(input$confirm_delete)) {
                    AppData$group_user_delete(
                        group_id = controlbar$group_selected(),
                        user_id = input$userToDelete
                    )
                    
                    rv$user_deleted <- rv$user_deleted + 1L
                    
                    alert_info(session, "Usuario eliminado")
                }
                
            }, error = \(e) alert_error(session, e))
        }) |> 
            bindEvent(input$confirm_delete)
        
        # Editing user ----
        
        observe({
            
            user_selected <- group_selected()[[input$userToEdit]]
            
            showModal(modalDialog(
                title = "Editar miembro",
                size = "l",
                
                fluidRow(
                    col_6(
                        textInput(
                            inputId = ns("user_editing"), 
                            label = "Selecciona usuario", 
                            value = paste(user_selected$name, user_selected$last_name),
                            width = "100%"
                        )
                    ),
                    col_2(
                        colourpicker::colourInput(
                            ns("color_editing"), 
                            "Color", 
                            palette = "limited", 
                            allowedCols = reportes_bs_colors() |> unlist(), 
                            value = reportes_bs_colors()[[user_selected$user_color]],
                            showColour = "background",
                            closeOnClick = TRUE
                        )
                    ),
                    col_4(
                        selectInput(
                            inputId = ns("role_editing"),
                            label = "Rol",
                            choices = c(Usuario = "user", Responsable = "admin"),
                            selected = user_selected$group_role
                        )
                    )
                ),
                
                footer = tagList(
                    modalButton("Cancelar"),
                    btn_guardar(ns("save_editing"))
                )
            ))
        }) |> 
            bindEvent(input$userToEdit)
        
        observe({
            tryCatch({
                user_selected <- group_selected()[[input$userToEdit]]
                color_selected <- reportes_bs_colors()[reportes_bs_colors() == input$color_editing] |> names()
                
                AppData$group_user_edit(
                    group_id = controlbar$group_selected(),
                    user_id = input$userToEdit,
                    user_color = color_selected,
                    group_role = input$role_editing
                )
                
                removeModal(session)
                
                rv$user_edited <- rv$user_edited + 1L
                module_output$group_colors_modified <- module_output$group_colors_modified + 1L
                
                alert_info(session, "Usuario editado")
                
            }, error = \(e) alert_error(session, e))
        }) |> 
            bindEvent(input$save_editing)
        
        ## output ----
        module_output
        
    })
}

## To be copied in the UI
# mod_group_users_ui("group_users_1")

## To be copied in the server
# mod_group_users_server("group_users_1")

mod_group_users_apptest <- function(id = "test") {
    
    AppData <- AppData$new()
    controlbar <- fake_controlbar(AppData)
    
    quick_bs4dash(
        modUI = mod_group_users_ui(id),
        modServer = mod_group_users_server(id, AppData, controlbar)
    )
}

