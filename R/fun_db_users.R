create_reporte_users <- function() {
  con <- db_connect()

  if (!DBI::dbExistsTable(con, "users")) {
    field_list <- data.frame(
      user_id = strrep(" ", 64),
      name = strrep(" ", 64),
      last_name = strrep(" ", 64),
      privileges = strrep(" ", 64),
      date_added = strrep(" ", 64)
    )

    DBI::dbWriteTable(con, "users", field_list)
    user_remove(user_id = strrep(" ", 64))
    message("created table 'users'")
  }

  DBI::dbDisconnect(con)
}

user_get_all <- function() {
    query <- "SELECT * from users"
    data <- db_get_query(query)
    return(data)
}

user_insert <- function(user_data) {
    con <- db_connect()
    DBI::dbWriteTable(con, "users", user_data, append = TRUE)
    DBI::dbDisconnect(con)
    user_id <- user_data$user_id
    glue::glue("inserted user with id '{user_id}'") |> message()
}

user_remove <- function(user_id) {
    statement <- "DELETE FROM users WHERE (user_id = {user_id})"
    db_execute_statement(statement, user_id = user_id)
    message(glue::glue("deleted user with id {user_id}"))
}

user_get_privileges <- function(user_id) {
    query <-    "SELECT privileges
                FROM users
                WHERE (user_id = {user_id})"
    data <- db_get_query(query, user_id = user_id)
    return(data$privileges)
}

user_get_from_privileges <- function(privileges) {
    query <-    "SELECT user_id
                FROM users
                WHERE (privileges IN ({vals*}))"
    data <- db_get_query(query, vals = privileges)
    return(data$user_id |> sort())
}

user_get_names <- function(user_id) {
    if (grepl("^team", user_id)) return(group_get_description(user_id))
    query <- "SELECT name, last_name
                            FROM users
                            WHERE (user_id IN ({vals*}))
                            ORDER BY user_id"
    data <- db_get_query(query, vals = user_id)
    return(paste(data$last_name, data$name, sep = ", "))
}

user_update <- function(user_id) {

}

user_create <- function(user_data, group_id = "team-dgco") {
    user_insert(user_data)
    data <- data.frame(user_id = user_data$user_id, group_id = group_id)
    gruser_insert(data)
}

user_get_choices <- function(user_id) {
    setNames(object = user_id,
             nm = user_id |> purrr::map_chr(user_get_names))
}


# db_remove_table("users")
# create_reporte_users()
#

# initial users dataset

# data.frame(
#   user_id = c("dgco93@mininter.gob.pe", "dgco26@mininter.gob.pe", "dgco84@mininter.gob.pe", "dgco80@mininter.gob.pe"),
#   name = c("Samuel", "Miguel", "Leanna", "Gelin"),
#   last_name =c("Calderon", "Rojas", "Zuñiga", "Espinoza"),
#   privileges = c("admin", "user1", "user1", "user1"),
#   responds_to = c("wolivos@mininter.gob.pe", "wolivos@mininter.gob.pe", "wolivos@mininter.gob.pe", "wolivos@mininter.gob.pe"),
#   date_added = rep(lubridate::today("America/Lima"), 4) |> as.character()
# ) |> user_insert()

# user_get_all()
