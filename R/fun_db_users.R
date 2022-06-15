create_reporte_users <- function() {
  con <- db_connect()

  if (!DBI::dbExistsTable(con, "users")) {
    field_list <- data.frame(
      user_id = strrep(" ", 64),
      name = strrep(" ", 64),
      last_name = strrep(" ", 64),
      privileges = strrep(" ", 64),
      responds_to = strrep(" ", 64),
      date_added = strrep(" ", 64)
    )

    DBI::dbWriteTable(con, "users", field_list)
    delete_user(user_id = strrep(" ", 64), with_print = FALSE)
    print("created table 'users'")
  }

  DBI::dbDisconnect(con)
}

insert_user <- function(user_data, with_print = TRUE) {
  con <- db_connect()
  DBI::dbWriteTable(con, "users", user_data, append = TRUE)
  DBI::dbDisconnect(con)
  if (with_print) print(sprintf("inserted user with id %s", user_data$user_id))
}

get_users <- function() {
  con <- db_connect()
  data <- DBI::dbReadTable(con, "users")
  DBI::dbDisconnect(con)
  return(data)
}

delete_user <- function(user_id, with_print = TRUE) {
  con <- db_connect()
  DBI::dbExecute(con, sprintf("DELETE FROM users WHERE (user_id = '%s')", user_id))
  DBI::dbDisconnect(con)
  if (with_print) print(sprintf("deleted user with id %s", user_id))
}

get_user_id_from_privileges <- function(privileges) {
  con <- db_connect()
  data <- DBI::dbGetQuery(con, sprintf("SELECT user_id FROM users WHERE (privileges = '%s')", privileges))
  DBI::dbDisconnect(con)
  return(data$user_id)
}

get_user_privilege_status <- function(user_id) {
  con <- db_connect()
  data <- DBI::dbGetQuery(con, sprintf("SELECT privileges FROM users WHERE (user_id = '%s')", user_id))
  DBI::dbDisconnect(con)
  return(data$privileges)
}


# remove_table_from_reporte("users")
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
# ) |> insert_user()

# get_users()
