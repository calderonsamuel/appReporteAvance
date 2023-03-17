report_box <- function(report, ns = NULL) {
    id <- ns_safe(report$report_id, ns)
    
    day_reported <- format(report$time_reported, "%d %b")
    time_reported <- format(report$time_reported, "%H:%M:%S")
    
    bs4Dash::box(
        title = report$report_title,
        width = 12,
        report$details,
        
        lapply(report$quantities, \(x){
            div(
                glue::glue("{output_unit}: {output_progress}", .envir = x)
            )
        }),
        
        glue::glue("Reportado el {day_reported} a las {time_reported}")
        
        
    )
}
