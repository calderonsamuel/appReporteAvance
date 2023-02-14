admin_toolbar <- function(inputId,
                          ns = NULL, 
                          edit_class,
                          delete_class,
                          float_class = "float-right"
                          ) {
    div(
        class = "btn-group", 
        class = float_class,
        role = "group",
        
        multi_btn_input(
            inputId = ns_safe(inputId, ns),
            label = fontawesome::fa("fas fa-pencil"),
            idForSelection = inputId,
            colorClass = "btn-warning",
            selector = edit_class,
            tooltip = "Editar"
        ),
        multi_btn_input(
            inputId = ns_safe(inputId, ns),
            label = fontawesome::fa("fas fa-trash"),
            idForSelection = inputId,
            colorClass = "btn-danger",
            selector = delete_class,
            tooltip = "Eliminar"
        )
    )
}
