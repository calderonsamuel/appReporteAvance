#' admin_groups UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_groups_ui <- function(id){
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
mod_groups_server <- function(id, user_iniciado){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    vals <- reactiveValues(
      users = user_get_all()$user_id |> setdiff("samuelcs8.17@gmail.com"),
      groups = group_get_all(),
      grusers_current = data.frame()
    )

    selected_group <- reactive({
        data <- vals$groups$group_id[input$tabla_rows_selected]
        message("updating selected_group()")
        return(data)
    })

    selected_user <- reactive({
        data <- grusers_tbl()$user_id[input$tabla_user_rows_selected]
        message("updating selected_user()")
        return(data)
    })

    grusers_choices <- reactive({
        availables <- setdiff(vals$users, vals$grusers_current) |> sort()
        data <- setNames(object = availables, nm = user_get_names(availables))
        message("updating grusers_choices()")
        return(data)
    })

    grusers_tbl <- reactive({
        data <- data.frame(
            user_id = vals$grusers_current,
            names = vals$grusers_current |> user_get_names()
        )
        message("updating grusers_tbl()")
        return(data)
    })

    new_group_data <- reactive({
        data <- data.frame(
            group_id = input$group_id,
            group_description = input$group_description)
        message("updating new_group_data()")
        return(data)
    })

    new_gruser_data <- reactive({
        data <- data.frame(
            group_id = selected_group(),
            user_id = input$user_id
        )
        message("updating new_gruser_data()")
        return(data)
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
      if (length(selected_group()) == 0) {
        alert_error(session, "Debe seleccionar un grupo a eliminar")
      } else {
        group_remove(selected_group())
        vals$groups <- group_get_all()
        alert_info(session = session, "Grupo eliminado")
      }
    })

    observeEvent(input$save, {
      group_insert(new_group_data())
      vals$groups <- group_get_all()
      removeModal()
      alert_success(session, "Grupo añadido")
    })

    observeEvent(input$user_edit, {
        message("pulsed user_edit")
      if (length(selected_group()) == 0) {
        alert_error(session, "Debe seleccionar un grupo a editar")
      } else {
          vals$grusers_current <- selected_group() |> gruser_get_from_group()
        showModal(modalDialog(
          title = "Administrar usuarios de grupo",

          DT::DTOutput(ns("tabla_user")),

          tags$br(),

          h5("Agregar usuario a grupo"),
          selectInput(
            inputId = ns("user_id"),
            label = "Usuarios",
            choices = grusers_choices(),
            multiple = TRUE

          ),
          btn_agregar(ns("add_user")),

          h5("Eliminar usuario de grupo"),
          btn_trash(ns("remove_user")),


          footer = modalButton("Cerrar")
        ))
      }
    })

    observe({
        message("pulsed add_user")
        if (isTruthy(input$user_id)) {
            gruser_insert(new_gruser_data())

            vals$grusers_current <- selected_group() |> gruser_get_from_group()

            alert_info(session, "Usuario añadido a grupo")
        } else {
            alert_error(session, "Debe escoger algún usuario")
        }

    }) |>
        bindEvent(input$add_user)

    observe({
        message("pulsed remove_user")
        if (isTruthy(selected_user())) {
            gruser_remove(group_id = selected_group(),
                          user_id = selected_user())

            vals$grusers_current <- selected_group() |> gruser_get_from_group()

            alert_info(session, "Usuario eliminado de grupo")
        } else {
            alert_error(session, "Debe seleccionar usuario")
        }
    }) |>
        bindEvent(input$remove_user)

    observe({
        updateSelectInput(
            session = session,
            inputId = "user_id",
            selected = NULL,
            choices = grusers_choices()
        )
    }) |>
        bindEvent(input$add_user, input$remove_user)



    output$tabla <- DT::renderDT(
      expr = vals$groups,
      options = options_DT(),
      selection = 'single'
    )

    output$tabla_user <- DT::renderDT(
      expr = grusers_tbl(),
      options = options_DT(),
      selection = 'single'
    )

  })
}

mod_groups_testapp <- function(id = "test") {
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
        bs4Dash::tabItem(tabName = "groups", mod_groups_ui(id))
      )
    )
  )



  server <- function(input, output, session) {
    mod_groups_server(id, user_iniciado = reactive("dgco93@mininter.gob.pe"))
  }

  shinyApp(ui, server, options = list(autoreload = TRUE))
}

## To be copied in the UI
# mod_groups_ui("admin_groups_1")

## To be copied in the server
# mod_groups_server("admin_groups_1")
