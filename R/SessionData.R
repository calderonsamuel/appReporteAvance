SessionData <- R6::R6Class(
    classname = "SessionData",
    public = list(
        user_id = NULL,
        user_names = NULL,
        privileges = NULL,
        groups = NULL,
        tasks = NULL,
        templates = NULL
    ),
    private = list(
        con = NULL
        # db_get_query = NULL,
        # db_execute_statement = NULL
    )
)

SessionData$set("private", "db_get_query", function(...) {
    dots <- list(...)
    dots[[".con"]] <- private$con
    query <- do.call(what = glue::glue_sql, args = dots)
    
    DBI::dbGetQuery(private$con, query)
})

SessionData$set("private", "db_execute_statement", function(...) {
    dots <- list(...)
    dots[[".con"]] <- private$con
    
    statement <- do.call(what = glue::glue_sql, args = dots)
    
    DBI::dbExecute(private$con, statement)
})

SessionData$set("private", "db_connect_remote", function() {
    DBI::dbConnect(
        drv = RMariaDB::MariaDB(),
        user = Sys.getenv("DB_USER"),
        password = Sys.getenv("DB_SECRET"),
        dbname = Sys.getenv("DB_NAME"),
        host = Sys.getenv("DB_HOST"),
        port = Sys.getenv("DB_PORT")
    )
})

SessionData$set("private", "db_connect_local", function(){
    DBI::dbConnect(
        drv = RSQLite::SQLite(),
        dbname = Sys.getenv("DB_NAME")
    )
})

SessionData$set("private", "get_privileges", function() {
    private$db_get_query("SELECT privileges
                              FROM users
                              WHERE (user_id = {self$user_id})")$privileges
})


SessionData$set("public", "initialize", function(user_id,
                                                 remote = getOption("reporte_avance.remote")) {
    message("Starting SessionData")
    
    private$con <- if (remote) private$db_connect_remote() else private$db_connect_local()
    self$user_id <- user_id
    self$privileges <- private$get_privileges()
    self$groups <- self$groups_compute()
    self$tasks <- NULL
    self$templates <- NULL
})

SessionData$set("public", "finalize", function() {
    message("Cleaning up SessionData")
    DBI::dbDisconnect(private$con)
})

# test <- SessionData$new("dgco93@mininter.gob.pe")
# 
# # test$db_get_query("SELECT * FROM users")
# # test$db_get_query("SELECT * FROM templates")
# rm(test)
# gc()
