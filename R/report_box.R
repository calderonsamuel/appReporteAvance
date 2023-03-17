report_box <- function(report, ns = NULL) {
    id <- ns_safe(report$report_id, ns)
    
    bs4Dash::box(
        title = report$report_title,
        width = 12,
        report$details,
        format(report$time_reported, "%d %b %H:%M:%S")
    )
}
