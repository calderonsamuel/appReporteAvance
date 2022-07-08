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
    user_remove(user_id = strrep(" ", 64), with_print = FALSE)
    message("created table 'users'")
  }

  DBI::dbDisconnect(con)
}

user_get_all <- function() {
    con <- db_connect()
    data <- DBI::dbReadTable(con, "users")
    DBI::dbDisconnect(con)
    return(data)
}

user_insert <- function(user_data) {
    con <- db_connect()
    DBI::dbWriteTable(con, "users", user_data, append = TRUE)
    DBI::dbDisconnect(con)
    message(sprintf("inserted user with id %s", user_data$user_id))
}

user_remove <- function(user_id) {
    con <- db_connect()
    DBI::dbExecute(con,
                   glue::glue_sql("DELETE FROM users WHERE (user_id = {user_id})", .con = con))
    DBI::dbDisconnect(con)
    message(glue::glue("deleted user with id {user_id}"))
}

user_get_privileges <- function(user_id) {
    con <- db_connect()
    query <- glue::glue_sql("SELECT privileges
                            FROM users
                            WHERE (user_id = {user_id})",
                            .con = con)
    data <- DBI::dbGetQuery(con, query)
    DBI::dbDisconnect(con)
    return(data$privileges)
}

user_get_from_privileges <- function(privileges) {
    con <- db_connect()
    query <- glue::glue_sql("SELECT user_id
                            FROM users
                            WHERE (privileges = {privileges})",
                            .con = con)
    data <- DBI::dbGetQuery(con, query)
    DBI::dbDisconnect(con)
    return(data$user_id)
}

user_get_names <- function(user_id, show_query = FALSE) {
    con <- db_connect()
    query <- glue::glue_sql("SELECT name, last_name
                            FROM users
                            WHERE (user_id IN ({vals*}))
                            ORDER BY user_id",
                            vals = user_id,
                            .con = con)
    data <- DBI::dbGetQuery(con, query)
    DBI::dbDisconnect(con)
    if (show_query) return(query)
    return(paste(data$last_name, data$name, sep = ", "))
}

user_update <- function(user_id) {
    con <- db_connect()
    DBI::dbExecute(con, "")
}

user_create <- function(user_data, group_id = "team-dgco") {
    user_insert(user_data)
    data <- data.frame(user_id = user_data$user_id, group_id = group_id)
    gruser_insert(data)
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
