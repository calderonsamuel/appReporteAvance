#' admin_groups UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_admin_groups_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::box(
      title = "Grupos disponibles",
      width = 12,
      sidebar = bs4Dash::boxSidebar(
        id = "sidebar",
        width = 25,
        btn_add(ns("add")),
        btn_trash(ns("remove"))
      ),
      DT::DTOutput(ns("tabla"))
    )
  )
}

#' admin_groups Server Functions
#'
#' @noRd
mod_admin_groups_server <- function(id, user_iniciado){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    vals <- reactiveValues(
      users = get_users(),
      groups = get_groups()
    )

    new_group_data <- reactive({
      data.frame(
        group_id = input$group_id,
        group_description = input$group_description,
        user_id = input$user_id
      )
    })

    selected_group_id <- reactive({
      vals$groups$group_id[input$tabla_rows_selected]
    })

    # current_groups <- reactiveVal(data.frame())

    observeEvent(input$add, {
      showModal(modalDialog(
        title = "Nuevo grupo",

        textInput(
          inputId = ns("group_id"),
          label = "ID grupo",
          placeholder = "A llenar por admin"
        ),
        textInput(
          inputId = ns("group_description"),
          label = "Nombre de grupo"
        ),
        selectInput(
          inputId = ns("user_id"),
          label = "Usuarios",
          choices = with(data = get_users(),
                         expr = setNames(object = user_id,
                                         nm = paste(name, last_name))),
          multiple = TRUE

        ),

        footer = tagList(
          modalButton("Cancelar"),
          btn_agregar(ns("save"))
        )
      ))
    })

    observeEvent(input$remove, {
      if (length(selected_group_id()) == 0) {
        alert_error(session, "Debe seleccionar un grupo a eliminar")
      } else {
        delete_group(selected_group_id())
        vals$groups <- get_groups()
        alert_info(session = session, "Grupo eliminado")
      }
    })

    observeEvent(input$save, {
      insert_group(new_group_data())
      vals$groups <- get_groups()
      removeModal()
      alert_success(session, "Grupo añadido")
    })

    output$tabla <- DT::renderDT(
      expr = vals$groups,
      options = options_DT(),
      selection = 'single'
    )

  })
}

mod_admin_groups_testapp <- function(id = "test") {
  ui <- bs4Dash::dashboardPage(
    header = bs4Dash::dashboardHeader(title = "TEST"),
    sidebar = bs4Dash::dashboardSidebar(
      bs4Dash::sidebarMenu(
        bs4Dash::menuItem(text = "Admin Groups",
                          tabName = "groups",
                          icon = icon("users"))
      )
    ),
    body = bs4Dash::dashboardBody(
      bs4Dash::tabItem(tabName = "groups", mod_admin_groups_ui(id))
    )
  )


  server <- function(input, output, session) {
    mod_admin_groups_server(id, user_iniciado = reactive("dgco93@mininter.gob.pe"))
  }

  shinyApp(ui, server)
}

## To be copied in the UI
# mod_admin_groups_ui("admin_groups_1")

## To be copied in the server
# mod_admin_groups_server("admin_groups_1")
