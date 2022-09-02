SessionData <- R6::R6Class(
    classname = "SessionData",
    public = list(
        con = NULL,
        user_iniciado = NULL,
        user_names = NULL,
        privileges = NULL,
        groups = NULL,
        tasks = NULL,
        templates = NULL,
        db_get_query = function(...) {
            dots <- list(...)
            dots[[".con"]] <- self$con
            query <- do.call(what = glue::glue_sql, args = dots)
            
            DBI::dbGetQuery(self$con, query)
        },
        db_execute_statement = function(...) {
            dots <- list(...)
            dots[[".con"]] <- self$con
            
            statement <- do.call(what = glue::glue_sql, args = dots)
            
            DBI::dbExecute(self$con, statement)
        }
    )
)

SessionData$set("public", "initialize", function(user_iniciado,
                                                 remote = getOption("reporte_avance.remote")) {
    message("Starting SessionData")
    self$user_iniciado <- user_iniciado
    self$con <- if (remote) {
        DBI::dbConnect(
            drv = RMariaDB::MariaDB(),
            user = Sys.getenv("DB_USER"),
            password = Sys.getenv("DB_SECRET"),
            dbname = Sys.getenv("DB_NAME"),
            host = Sys.getenv("DB_HOST"),
            port = Sys.getenv("DB_PORT")
        )
    } else {
        DBI::dbConnect(drv = RSQLite::SQLite(),
                       dbname = Sys.getenv("DB_NAME"))
    }
    self$privileges <- self$db_get_query("SELECT privileges
                              FROM users
                              WHERE (user_id = {self$user_iniciado})")$privileges
})

SessionData$set("public", "finalize", function() {
    message("Cleaning up SessionData")
    DBI::dbDisconnect(self$con)
})

# test <- SessionData$new("dgco93@mininter.gob.pe")
# 
# # test$db_get_query("SELECT * FROM users")
# # test$db_get_query("SELECT * FROM templates")
# rm(test)
# gc()
