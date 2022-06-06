create_reporte_users <- function() {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), "reporte_avance.db")

  if (!RSQLite::dbExistsTable(con, "users")) {
    field_list <- data.frame(
      user_id = character(),
      name = character(),
      last_name = character(),
      privileges = character(),
      # contract_type = character(),
      date_added = character()
    )

    RSQLite::dbWriteTable(con, "users", field_list)
    print("created table 'users'")
  }

  RSQLite::dbDisconnect(con)
}

insert_user <- function(user_data) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), "reporte_avance.db")
  RSQLite::dbWriteTable(con, "users", user_data, append = TRUE)
  RSQLite::dbDisconnect(con)
  print(sprintf("inserted user with id %s", user_data$user_id))
}

get_users <- function() {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), "reporte_avance.db")
  data <- RSQLite::dbReadTable(con, "users")
  RSQLite::dbDisconnect(con)
  return(data)
}

delete_user <- function(user_id) {
  con <- RSQLite::dbConnect(RSQLite::SQLite(), "reporte_avance.db")
  RSQLite::dbExecute(con, sprintf("DELETE FROM users WHERE (user_id = '%s')", user_id))
  RSQLite::dbDisconnect(con)
  print(sprintf("deleted user with id %s", user_id))
}



# users <- data.frame(
#   user_id = c("dgco93", "dgco26", "dgco84"),
#   name = c("Samuel", "Miguel", "Leanna"),
#   last_name =c("Calderon", "Rojas", "Zuñiga"),
#   # contract_type = c("OS", "OS", "OS"),
#   privileges = c("admin", "user", "user"),
#   date_added = rep(lubridate::today("America/Lima"), 3) |> as.character()
# )

# create_reporte_users()
# remove_table_from_reporte("users")

# insert_user(users)
# get_users()
