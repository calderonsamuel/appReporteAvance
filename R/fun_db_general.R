db_connect <- function() {
  DBI::dbConnect(
    drv = RMariaDB::MariaDB(),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_SECRET"),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    port = Sys.getenv("DB_PORT"))
}

db_list_tables <- function() {
  con <- db_connect()
  tables <- DBI::dbListTables(con)
  DBI::dbDisconnect(con)
  return(tables)
}

remove_table_from_reporte <- function(table) {
  con <- db_connect()
  DBI::dbRemoveTable(con, table)
  DBI::dbDisconnect(con)
  message(sprintf("deleted table '%s'", table))
}
