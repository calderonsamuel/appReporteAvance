#' Clase R6 para manejar la base de datos
#'
#' Una base de datos puede ejecutar y obtener querys
DBManager <- R6::R6Class(
    classname = "DBManager",
    public = list(
        initialize = function() {
            message("Starting DB connection")
            private$con <-  private$db_connect()
        },
        finalize = function() {
            message("Finalizing DB connection")
            DBI::dbDisconnect(private$con)
        },
        db_execute_statement = function(...) {
            dots <- list(...)
            dots[[".con"]] <- private$con
            
            statement <- do.call(what = glue::glue_sql, args = dots)
            
            DBI::dbExecute(private$con, statement)
        },
        db_get_query = function(...) {
            dots <- list(...)
            dots[[".con"]] <- private$con
            
            query <- do.call(what = glue::glue_sql, args = dots)
            
            DBI::dbGetQuery(private$con, query)
        }
    ),
    private = list(
        con = NULL,
        db_connect = function() {
            DBI::dbConnect(
                drv = RMariaDB::MariaDB(),
                user = Sys.getenv("DB_USER"),
                password = Sys.getenv("DB_SECRET"),
                dbname = Sys.getenv("DB_NAME"),
                host = Sys.getenv("DB_HOST"),
                port = Sys.getenv("DB_PORT")
            )  
        }
    )
)
