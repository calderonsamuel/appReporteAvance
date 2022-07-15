.onLoad <- function(libname, pkgname) {
    op <- options()
    op.reporte_avance <- list(
        reporte_avance.remote = TRUE
    )
    toset <- !(names(op.reporte_avance) %in% names(op))
    if(any(toset)) options(op.reporte_avance[toset])

    invisible()
}
