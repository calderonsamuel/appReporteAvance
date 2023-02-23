multiBtnInput <- function(inputId, value, label, 
                          icon = NULL, width = NULL, tooltip = NULL, ...) {
    
    input_tag <- htmltools::tags$button(
        id = ids::random_id(n = 1),
        type = "button",
        style = htmltools::css(width = htmltools::validateCssUnit(width)),
        `id-for-selection` = inputId,
        `multi-value` = value,
        class = "btn multi-btn",
        icon,
        label,
        ...
    )
    
    if (!is.null(tooltip)) {
        input_tag <- input_tag |> add_tooltip(text = tooltip)
    }
    
    tagList(
        multiBtnInput_dep(),
        input_tag
    )
}

add_tooltip <- function(tag, text, placement = "top") {
    tag |> 
        htmltools::tagAppendAttributes(
            `data-toggle`="tooltip",
            `data-placement`= placement,
            title = text
        )
}

multiBtnInput_dep <- function() {
    htmltools::htmlDependency(
        name = "multiBtnShinyInput", 
        version = "1.0",
        package = "appReporteAvance",
        src = "js",
        script = "multiBtnShinyInput.js"
    )
}
