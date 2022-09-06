## Example shiny app to create a plot from sortable inputs

library(shiny)
library(htmlwidgets)
library(sortable)
library(magrittr)
library(bs4Dash)

ui <- dashboardPage(
    header = dashboardHeader(title = "TEST"),
    sidebar = dashboardSidebar(disable = TRUE),
    body = dashboardBody(
        div(
            class = "panel-heading",
            h3("Dragging variables to define a plot")
        ),
        fluidRow(
            class = "panel panel-heading",
            
            fluidRow(
                class = "panel-body",
                column(
                    width = 3,
                    bs4Dash::box(
                        title = "Box Principal",
                        width = 12,
                        
                        tags$div(
                            id = "sort1",
                            box(title = "box1", id = "b1"),
                            box(title = "box2", id = "b2"),
                            box(title = "box3", id = "b3"),
                            # colnames_to_tags(mtcars)
                        )
                    )
                ),
                column(
                    width = 3,
                    box(
                        title = "X",
                        width = 12,
                        tags$div(
                            id = "sort2"
                        )
                    ),
                    box(
                        title = "Y",
                        width = 12,
                        tags$div(
                            id = "sort3"
                        )
                    ),
                ),
                column(
                    width = 6,
                    verbatimTextOutput("debug")

                )
            ) |> column(width = 12)
        ),
        sortable_js(
            "sort1",
            options = sortable_options(
                group = list(
                    name = "sortGroup1",
                    put = TRUE
                ),
                sort = FALSE,
                onSort = sortable_js_capture_input("sort_vars"),
                onLoad = sortable_js_capture_input("sort_vars")
            )
        ),
        sortable_js(
            "sort2",
            options = sortable_options(
                group = list(
                    group = "sortGroup1",
                    put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
                    pull = TRUE
                ),
                onSort = sortable_js_capture_input("sort_x"),
                onLoad = sortable_js_capture_input("sort_x")
            )
        ),
        sortable_js(
            "sort3",
            options = sortable_options(
                group = list(
                    group = "sortGroup1",
                    put = htmlwidgets::JS("function (to) { return to.el.children.length < 1; }"),
                    pull = TRUE
                ),
                onSort = sortable_js_capture_input("sort_y"), 
                onLoad = sortable_js_capture_input("sort_y")
            )
        )
    )
)

server <- function(input, output) {
    
    x <- reactive({
        x <- input$sort_x
        if (is.character(x)) x %>% trimws()
    })
    
    y <- reactive({
        input$sort_y %>% trimws()
    })
    
    output$debug <- renderPrint(list(
        sort_vars = input$sort_vars,
        x = input$sort_x,
        y = input$sort_y
    ))
    
}
shinyApp(ui, server)
