isFalsy <- function(...) {
    dots <- list(...)
    vec <- logical(length(dots))
    for (i in seq_along(dots)) {
        vec[i] <- !isTruthy(dots[[i]])
    }
    any(vec)
}
