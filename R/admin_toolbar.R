admin_toolbar <- function(editInputId, 
                          deleteInputId,
                          value,
                          float_class = "float-right"
                          ) {
    div(
        class = "btn-group", 
        class = float_class,
        role = "group",
        
        multiBtnInput(
            inputId = editInputId,
            value = value,
            label = fontawesome::fa("fas fa-pencil"),
            class = "btn-warning",
            tooltip = "Editar"
        ),
        multiBtnInput(
            inputId = deleteInputId,
            value = value,
            label = fontawesome::fa("fas fa-trash"),
            class = "btn-danger",
            tooltip = "Eliminar"
        )
    )
}
