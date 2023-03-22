textInputPro <- function(inputId, label, value = "", width = NULL, 
                       placeholder = NULL, maxlength = NULL, maxlengthCounter = FALSE) {
    
    tag <- shiny::textInput(
        inputId = inputId,
        label = label,
        value = value,
        width = width,
        placeholder = placeholder
    )

    if (is.null(maxlength)) return(tag)

    tq <- htmltools::tagQuery(tag)$
            children("input")$
            addAttrs(maxlength=maxlength)
        
    if (maxlengthCounter) {
        element_counter <- htmltools::tags$small(
            id = paste0(inputId, "MlCounter"),
            class = "form-text text-muted",
            paste0("0 / ", maxlength)
        )
        tq <- tq$after(element_counter)
    }
    tagList(
        maxlength_dep(),
        tq$allTags()
    )
    
    
    
}

maxlength_dep <- function() {
    htmltools::htmlDependency(
        name = "maxlength",
        version = "0.1.0",
        package = "appReporteAvance",
        src = "js",
        script = "maxlength.js"
    )
}

bslib::page_fluid(
    tags$head(
        # htmltools::htmlDependency(
        #     name = "maxlength",
        #     version = "0.1.0",
        #     package = "appReporteAvance",
        #     src = "js",
        #     script = "maxlength.js"
        # )
        includeScript("inst/js/maxlength.js")
    ),
    textInputPro("hi", "hi", maxlength = 25, maxlengthCounter = TRUE)
)
