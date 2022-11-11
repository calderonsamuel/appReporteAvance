quick_bs4dash <- function(..., modUI = NULL, modServer = NULL) {
    ui <- bs4Dash::dashboardPage(
        freshTheme = custom_theme(),
        header = bs4Dash::dashboardHeader(title = "quick bs4dash"),
        sidebar = bs4Dash::dashboardSidebar(
            bs4Dash::sidebarMenu(
                bs4Dash::menuItem(text = "A Tab",
                                  tabName = "tab-a",
                                  icon = icon("tasks"))
            )
        ),
        body = bs4Dash::dashboardBody(
            golem_add_external_resources(),
            bs4Dash::tabItem(tabName = "tab-a", modUI, ...)
        )
    )
    
    server <- function(input, output, session) {
        modServer
    }
    
    shinyApp(ui, server)
}
