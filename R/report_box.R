report_box <- function(report, ns = NULL) {
    id <- ns_safe(report$report_id, ns)
    
    day_reported <- format(report$time_reported, "%d %b")
    time_reported <- format(report$time_reported, "%H:%M:%S")
    
    dropdown <- report_dropdown(ns, report$report_id)
    
    bs4Dash::box(
        title = report$report_title,
        width = 12,
        collapsed = TRUE,
        dropdownMenu = dropdown,
        
        report$details,
        
        lapply(report$quantities, \(x){
            tags$p(
                glue::glue("{output_unit}: {output_progress}", .envir = x)
            )
        }),
        
        tags$p(
            class = "text-muted small mb-0",
            glue::glue("Reportado el {day_reported} a las {time_reported}")
        )
    )
}

report_dropdown <- function(ns, value) {
    item_archivar <- multiBtnInput(
        inputId = ns_safe("reportToArchive", ns),
        value = value,
        label = "Archivar",
        icon = fontawesome::fa("fas fa-box-archive"),
        class = "dropdown-item"
    )
    
    item_eliminar <- multiBtnInput(
        inputId = ns_safe("reportToDelete", ns), 
        value = value,
        label = tags$span(fontawesome::fa("fas fa-trash", fill = "#cf222e"), 
                          "Eliminar", style = "color: #cf222e;"),
        class = "dropdown-item"
    )
    
    bs4Dash::boxDropdown(
        icon = fontawesome::fa("fas fa-ellipsis"), 
        tagList(
            item_archivar,
            item_eliminar
        )
        
    )
}
