multi_btn_input <- function(
        inputId, label, 
        selectorClass, 
        idForSelection = "",
        colorClass = "",
        tooltip = NULL
) {
    
    input_tag <-  htmltools::tags$button(
        id = inputId, 
        type = "button",
        label, 
        `id-for-selection` = idForSelection,
        class = "btn",
        class = colorClass,
        class = selectorClass
    )
    
    if (!is.null(tooltip)) {
        input_tag <- input_tag |> 
            tagAppendAttributes(
                `data-toggle`="tooltip",
                `data-placement`="top",
                title=tooltip
            )
    }
    
    tagList(
        multi_btn_input_dep(), 
        input_tag
    )
}


multi_btn_input_dep <- function() {
    htmltools::htmlDependency(
        name = "multiBtnShinyInput", 
        version = "1.0",
        package = "appReporteAvance",
        src = "js",
        script = "multiBtnShinyInput.js"
    )
}

