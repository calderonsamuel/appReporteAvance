#' Clase R6 para manejar la base de datos
#'
#' Una base de datos puede ejecutar y obtener querys
DBManager <- R6::R6Class(
    classname = "DBManager",
    public = list(
        con = NULL,
        db_execute_statement = NULL,
        db_get_query = NULL
    ),
    private = list(
        
    )
)
