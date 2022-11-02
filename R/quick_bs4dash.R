quick_bs4dash <- function(...) {
    ui <- bs4Dash::dashboardPage(
        header = bs4Dash::dashboardHeader(title = "quick bs4dash"),
        sidebar = bs4Dash::dashboardSidebar(
            bs4Dash::sidebarMenu(
                bs4Dash::menuItem(text = "A Tab",
                                  tabName = "tab-a",
                                  icon = icon("tasks"))
            )
        ),
        body = bs4Dash::dashboardBody(
            tags$head(
            ),
            bs4Dash::tabItem(tabName = "tab-a", ...)
        )
    )
    
    server <- function(input, output, session) {
        
    }
    
    shinyApp(ui, server)
}
