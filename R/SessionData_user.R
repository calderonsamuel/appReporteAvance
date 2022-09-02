SessionData$set("public", "user_get_all", function() {
    private$db_get_query("SELECT * FROM users")
})

SessionData$set("public", "user_insert", function(user_data) {
    DBI::dbWriteTable(private$con, "users", user_data, append = TRUE)
    glue::glue("inserted user with id '{user_data$user_id}'") |> message()
})

SessionData$set("public", "user_remove", function(user_id) {
    statement <- "DELETE FROM users WHERE (user_id = {user_id})"
    private$db_execute_statement(statement, user_id = user_id)
    message(glue::glue("deleted user with id {user_id}"))
})

SessionData$set("public", "user_get_privileges", function() {
    data <- private$db_get_query("SELECT privileges 
                              FROM users
                              WHERE (user_id = {self$user_id})")
    data$privileges
})

SessionData$set("public", "user_get_from_privileges", function(privileges) {
    query <-    "SELECT user_id
                FROM users
                WHERE (privileges IN ({vals*}))"
    data <- db_get_query(query, vals = privileges)
    return(data$user_id |> sort())
})

SessionData$set("public", "user_get_names", function(user_id) {
    if (grepl("^team", user_id)) return(self$group_get_description(user_id))
    query <- "SELECT name, last_name
                            FROM users
                            WHERE (user_id IN ({vals*}))
                            ORDER BY user_id"
    data <- private$db_get_query(query, vals = user_id)
    return(paste(data$last_name, data$name, sep = ", "))
})

SessionData$set("public", "user_is_registered", function(user_id) {
    query <- "SELECT user_id
                FROM users
                WHERE user_id = {user_id}"
    data <- private$db_get_query(query, user_id = user_id)
    return(shiny::isTruthy(data$user_id))
})

SessionData$set("public", "user_get_choices", function(user_id) {
    setNames(object = user_id,
             nm = user_id |> purrr::map_chr(self$user_get_names))
})

# testing

# test <- SessionData$new("dgco93@mininter.gob.pe")
# 
# test$user_insert(data.frame(
#     user_id = "testing",
#     name = "testing",
#     last_name = "testing",
#     privileges = "user1",
#     responds_to = "testing",
#     date_added = as.character(lubridate::today("America/Lima"))
# ))
# 
# test$user_remove("testing")

# test$user_get_names("team-politicas")

# test$user_is_registered("dgco84@mininter.gob.pe")

# rm(test)
# gc()
