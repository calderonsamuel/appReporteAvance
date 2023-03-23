#' Create a text input with enhanced parameters
#' 
#' @inheritParams shiny::textInput
#' @param maxlength the maximum length of the input
#' @param maxlengthCounter whether or not to show a character counter
#' 
#' @return Returns a text input with the specified parameters.
textInputPro <- function(inputId, label, value = "", width = NULL,
        placeholder = NULL, maxlength = NULL, maxlengthCounter = FALSE,
        readonly = FALSE) {
    
    tag <- shiny::textInput(
        inputId = inputId,
        label = label,
        value = value,
        width = width,
        placeholder = placeholder
    )

    if (readonly) {
        tag <- tag_add_readonly(tag, "input")
    }

    if (is.null(maxlength)) return(tag)

    tag <- tag_add_maxlength(tag, maxlength, type = "input")

    if (!maxlengthCounter) return(tag)
        
    tag <- tag_add_element_counter(tag, value, maxlength)
    
    tagList(
        maxlength_dep(),
        tag
    )
}


#' Create a text area input with enhanced parameters
#' 
#' @inheritParams textInputPro
#' @inheritParams shiny::textAreaInput
#' @return Returns a text area input with the specified parameters.
textAreaInputPro <- function(inputId, label, value = "", 
        width = NULL, height = NULL, cols = NULL, rows = NULL,
        placeholder = NULL, resize = NULL,
        maxlength = NULL, maxlengthCounter = FALSE,
        readonly = FALSE) {

    tag <- shiny::textAreaInput(
        inputId = inputId,
        label = label,
        value = value,
        width = width,
        height = height,
        cols = cols,
        rows = rows,
        placeholder = placeholder,
        resize = resize
    )

    if (readonly) {
        tag <- tag_add_readonly(tag, "textarea")
    }

    if (is.null(maxlength)) return(tag)

    tag <- tag_add_maxlength(tag, maxlength, type = "textarea")

    if (!maxlengthCounter) return(tag)

    tag <- tag_add_element_counter(tag, value, maxlength)
    
    tagList(
        maxlength_dep(),
        tag
    )
}

maxlength_dep <- function() {
    htmltools::htmlDependency(
        name = "maxLengthCounter",
        version = "0.1.0",
        package = "appReporteAvance",
        src = "js",
        script = "maxLengthCounter.js"
    )
}

tag_add_readonly <- function(tag, type) {
    match.arg(type, c("input", "textarea"))
    
    htmltools::tagQuery(tag)$
        children(type)$
        addAttrs(readonly = "")$
        allTags()
}


tag_add_maxlength <- function(tag, maxlength, type) {
    match.arg(type, c("input", "textarea"))

    htmltools::tagQuery(tag)$
        children(type)$
        addAttrs(maxlength = maxlength)$
        allTags()
}

tag_add_element_counter <- function(tag, value, maxlength) {
    element_counter <- htmltools::tags$small(
        class = "form-text text-muted",
        paste0(nchar(value), " / ", maxlength)
    )
    
    htmltools::tagQuery(tag)$append(element_counter)$allTags()
}

# bslib::page_fluid(
#     htmltools::tags$head(
#         htmltools::htmlDependency(
#             name = "maxlength",
#             version = "0.1.0",
#             package = "appReporteAvance",
#             src = "js",
#             script = "maxlength.js"
#         )
#         # shiny::includeScript("inst/js/maxlength.js")
#     ),
#     textInputPro("hi", "hi", maxlength = 25, maxlengthCounter = TRUE),
#     textInputPro("hi2", "hi2", maxlength = 25),
#     textAreaInputPro("hi3", "hi3", maxlength = 25, maxlengthCounter = TRUE),
#     textAreaInputPro("hi4", "hi4", maxlength = 25)
# )
