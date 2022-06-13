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


# remove_table_from_reporte("users")
# create_reporte_users()
#
# users <- data.frame(
#   user_id = c("dgco93", "dgco26", "dgco84"),
#   name = c("Samuel", "Miguel", "Leanna"),
#   last_name =c("Calderon", "Rojas", "Zuñiga"),
#   privileges = c("admin", "user", "user"),
#   date_added = rep(lubridate::today("America/Lima"), 3) |> as.character()
# )
#
# insert_user(users)
# get_users()
