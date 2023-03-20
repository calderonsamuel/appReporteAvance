#' Clase R6 para manejar la base de datos
#'
#' Una base de datos puede ejecutar y obtener querys
#'
#' @param use_tibble Whether print the results of DbGetQuery() as a tibble or not. Default TRUE
#' @param ... Objects passed to glue::glue_sql() with the exception of the `.con` argument.
#' @export
#' @importFrom DBI dbConnect dbExecute dbGetQuery dbDisconnect
#' @importFrom glue glue_sql
#' @importFrom cli cli_alert_info
#' @importFrom tibble as_tibble
#' @importFrom lubridate now
#' @importFrom R6 R6Class
#' @importFrom RMariaDB MariaDB
DBManager <- R6::R6Class(
    classname = "DBManager",
    public = list(
        #' @description Start the DB Manager
        initialize = function(use_tibble = TRUE) {
            private$con <-  private$db_connect()
            private$use_tibble <- use_tibble
            
            if (interactive()) cli::cli_alert_info("Connected to DB")
        },
        #' @description Analog to  `DBI::dbExecute()`
        db_execute_statement = function(...) {
            dots <- list(...)
            dots[[".con"]] <- private$con

            statement <- do.call(what = glue::glue_sql, args = dots)

            DBI::dbExecute(private$con, statement)
        },
        #' @description Analog to  `DBI::dbGetQuery()`
        db_get_query = function(...) {
            dots <- list(...)
            dots[[".con"]] <- private$con

            query <- do.call(what = glue::glue_sql, args = dots)

            data_returned <- DBI::dbGetQuery(private$con, query)

            if (private$use_tibble) tibble::as_tibble(data_returned) else data_returned
        },
        #' @description Construct the query to be passed to `DBI::dbGetQuery()`. Useful for debugging and subquery construction.
        db_make_query = function(...) {
            dots <- list(...)
            dots[[".con"]] <- private$con

            do.call(what = glue::glue_sql, args = dots)
        }
    ),
    private = list(
        con = NULL,
        use_tibble = NULL,
        db_connect = function() {
            DBI::dbConnect(
                drv = RMariaDB::MariaDB(),
                user = Sys.getenv("DB_USER"),
                password = Sys.getenv("DB_SECRET"),
                dbname = Sys.getenv("DB_NAME"),
                host = Sys.getenv("DB_HOST"),
                port = Sys.getenv("DB_PORT")
            )
        },
        finalize = function() {
            DBI::dbDisconnect(private$con)
            if (interactive()) cli::cli_alert_info("Ending DB connection")
        }
    )
)
