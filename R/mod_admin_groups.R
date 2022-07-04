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
    # waiter::autoWaiter(),
    bs4Dash::box(
      title = "Grupos disponibles",
      width = 12,
      sidebar = bs4Dash::boxSidebar(
        id = ns("sidebar"),
        width = 25,
        h5("Administrar grupos"),
        btn_add(ns("add")),
        btn_trash(ns("remove")),
        h5("Administrar usuarios"),
        btn_user_edit(ns("user_edit"))
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
      users = user_get_all(),
      groups = get_groups()
    )

    new_group_data <- reactive({
      data.frame(
        group_id = input$group_id,
        group_description = input$group_description
        # user_id = input$user_id
      )
    })

    selected_group_id <- reactive({
      vals$groups$group_id[input$tabla_rows_selected]
    })

    selected_user_id <- reactive({
      vals$users$user_id[input$tabla_user_rows_selected]
    })

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

    observeEvent(input$user_edit, {
      if (length(selected_group_id()) == 0) {
        alert_error(session, "Debe seleccionar un grupo a editar")
      } else {
        showModal(modalDialog(
          title = "Administrar usuarios de grupo",

          DT::DTOutput(ns("tabla_user")),

          tags$br(),

          h5("Agregar usuario a grupo"),
          selectInput(
            inputId = ns("user_id"),
            label = "Usuarios",
            choices = with(data = user_get_all(),
                           expr = setNames(object = user_id,
                                           nm = paste(name, last_name))),
            multiple = TRUE

          ),
          btn_agregar(ns("add_user")),

          h5("Eliminar usuario de grupo"),
          btn_trash(ns("remove_user")),


          footer = modalButton("Cerrar")
        ))
      }
    })

    observeEvent(input$add_user,{



    })

    observeEvent(input$save_user, {
      removeModal()
      alert_success(session, "Usuario añadido")
    })

    output$tabla <- DT::renderDT(
      expr = vals$groups,
      options = options_DT(),
      selection = 'single'
    )

    output$tabla_user <- DT::renderDT(
      expr = get_group_users_metadata(selected_group_id()),
      options = options_DT(),
      selection = 'single'
    )

  })
}

mod_admin_groups_testapp <- function(id = "test") {
  ui <- tagList(
    bs4Dash::dashboardPage(
      # preloader = list(html = waiter::spin_1(), color = "#333e48"),
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
